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
                    htlc = mkHTLC ctx secretHash testWallet2PublicKey testWalletPublicKey testWallet2PublicKey lockedUntil
                (fundTx, fundTxSelectIns) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ fundHTLC htlc (btcToSatoshi 10) (Just 3) -- Create three outputs: one to redeem, two to test refunds with both keys.
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
                    scriptUTxORefund1 = scriptUTxOs !! 1
                    scriptUTxORefund2 = scriptUTxOs !! 2
                (redeemTx, _redeemTxSelectIns, sigHashRedeem) <- runBitcoinBuilderMonadIO provider [testWalletAddress2] testWalletAddress2 $ redeemHTLC scriptUTxORedeem htlc
                step $ "sigHashRedeem: " <> show sigHashRedeem
                (redeemSignedTx, redeemTxId) <- runBitcoinQueryMonadIO provider $ signAndSubmitRedeemHTLC ctx testWalletXPrvKey2.key sigHashRedeem secret htlc redeemTx
                step $ "redeemSignedTx: " <> show redeemSignedTx
                step $ "redeemTxId: " <> show redeemTxId
                (refundTx1, _refundTxSelectIns1, sigHashRefund1) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ refundHTLC scriptUTxORefund1 htlc
                (refundSignedTx1, refundTxId1) <- runBitcoinQueryMonadIO provider $ signAndSubmitRefundHTLC ctx testWalletXPrvKey.key sigHashRefund1 htlc refundTx1
                step $ "refundSignedTx1: " <> show refundSignedTx1
                step $ "refundTxId1: " <> show refundTxId1
                (refundTx2, _refundTxSelectIns2, sigHashRefund2) <- runBitcoinBuilderMonadIO provider [testWalletAddress2] testWalletAddress2 $ refundHTLC scriptUTxORefund2 htlc
                (refundSignedTx2, refundTxId2) <- runBitcoinQueryMonadIO provider $ signAndSubmitRefundHTLCAlt ctx testWalletXPrvKey2.key sigHashRefund2 htlc refundTx2
                step $ "refundSignedTx2 (alt pubkey): " <> show refundSignedTx2
                step $ "refundTxId2 (alt pubkey): " <> show refundTxId2
        ]