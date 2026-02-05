{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Data.Function ((&))
import Data.Text.Encoding (encodeUtf8)
import Haskoin (hexToTxHash, sha256, withContext, wrapPubKey)
import Haskoin qualified
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinQueryMonad (..))
import ZkFold.Bitcoin.Examples.HTLC
import ZkFold.Bitcoin.IO
import ZkFold.Bitcoin.Test.Constants (testWalletAddress, testWalletAddress2, testWalletXPrvKey, testWalletXPrvKey2, testWalletXPubKey, testWalletXPubKey2)
import ZkFold.Bitcoin.Types

providerConfig :: BitcoinProviderConfig
providerConfig =
    BPCNode
        ( BitcoinProviderConfigNode
            { bpcnUsername = "user"
            , bpcnPassword = "password"
            , bpcnUrl = "http://localhost:18443"
            , bpcnNetworkId = RegTest
            }
        )

regTestTests :: TestTree
regTestTests =
    testGroup
        "regtest"
        [ testCase "txConfirmations on non-existent transaction should be 0" $ do
            provider <- providerFromConfig providerConfig
            txHash <- case hexToTxHash "0000000000000000000000000000000000000000000000000000000000000000" of
                Nothing -> assertFailure "invalid tx hash literal"
                Just txHash -> pure txHash
            confirmations <- runBitcoinQueryMonadIO provider $ txConfirmations txHash
            assertEqual
                "txConfirmations on non-existent transaction should be 0"
                0
                confirmations
        , testCaseSteps "HTLC send" $ \step -> do
            provider <- providerFromConfig providerConfig
            let secret = encodeUtf8 "mysecret"
                secretHash = sha256 secret
            currentBlockHeight <- runBitcoinQueryMonadIO provider blockCount
            let lockedUntil = currentBlockHeight + 1
            step $ "currentBlockHeight: " <> show currentBlockHeight <> " lockedUntil: " <> show lockedUntil
            withContext $ \ctx -> do
                let testWallet2PublicKey = testWalletXPubKey2.key & wrapPubKey True
                    testWalletPublicKey = testWalletXPubKey.key & wrapPubKey True
                    htlc = mkHTLC ctx secretHash testWallet2PublicKey testWalletPublicKey lockedUntil
                (fundTx, fundTxSelectIns) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ fundHTLC htlc (btcToSatoshi 10) (Just 2) -- Create two outputs, one for testing redeem and other for the refund.
                (fundSignedTx, fundTxId) <- runBitcoinSignerMonadIO provider [testWalletAddress] testWalletAddress [testWalletXPrvKey.key] $ signAndSubmitFundHTLC (fundTx, fundTxSelectIns)
                step $ "fundSignedTx: " <> show fundSignedTx
                step $ "fundTxId: " <> show fundTxId
                let confirmations = 2
                runBitcoinQueryMonadIO provider $
                    waitForTxConfirmations
                        fundTxId
                        ( TxConfirmationsConfig
                            { tccConfirmations = confirmations
                            , tccPollIntervalSeconds = 1
                            , tccMaxAttempts = 30
                            }
                        )
                currentBlockHeight' <- runBitcoinQueryMonadIO provider blockCount
                step $ "currentBlockHeight': " <> show currentBlockHeight'
                assertEqual
                    "block height advanced by tccConfirmations"
                    (currentBlockHeight + confirmations)
                    currentBlockHeight'
                scriptUTxOs <- runBitcoinQueryMonadIO provider $ utxosAtAddress htlc.htlcAddress
                step $ "scriptUTxOs: " <> show scriptUTxOs
                let scriptUTxORedeem = head scriptUTxOs
                    scriptUTxORefund = scriptUTxOs !! 1
                (redeemTx, _redeemTxSelectIns, sigHashRedeem) <- runBitcoinBuilderMonadIO provider [testWalletAddress2] testWalletAddress2 $ redeemHTLC scriptUTxORedeem htlc
                step $ "sigHashRedeem: " <> show sigHashRedeem
                (redeemSignedTx, redeemTxId) <- runBitcoinQueryMonadIO provider $ signAndSubmitRedeemHTLC ctx testWalletXPrvKey2.key sigHashRedeem secret htlc redeemTx
                step $ "redeemSignedTx: " <> show redeemSignedTx
                step $ "redeemTxId: " <> show redeemTxId
                (refundTx, _refundTxSelectIns, sigHashRefund) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ refundHTLC scriptUTxORefund htlc
                (refundSignedTx, refundTxId) <- runBitcoinQueryMonadIO provider $ signAndSubmitRefundHTLC ctx testWalletXPrvKey.key sigHashRefund htlc refundTx
                step $ "refundSignedTx: " <> show refundSignedTx
                step $ "refundTxId: " <> show refundTxId
        ]