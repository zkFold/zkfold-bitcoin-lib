{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Control.Concurrent (threadDelay)
import ZkFold.Bitcoin.Examples.HTLC
import Data.Function ((&))
import Data.Text.Encoding (encodeUtf8)
import Haskoin (sha256, withContext, wrapPubKey)
import Haskoin qualified
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinQueryMonad (..))
import ZkFold.Bitcoin.IO
import ZkFold.Bitcoin.Test.Constants (testWalletAddress, testWalletAddress2, testWalletXPrvKey, testWalletXPrvKey2, testWalletXPubKey, testWalletXPubKey2)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (btcToSatoshi)

regTestTests :: TestTree
regTestTests =
  testGroup
    "regtest"
    [ testCaseSteps "HTLC send" $ \step -> do
        let providerConfig =
              BPCNode
                ( BitcoinProviderConfigNode
                    { bpcnUsername = "user"
                    , bpcnPassword = "password"
                    , bpcnUrl = "http://localhost:18443"
                    , bpcnNetworkId = RegTest
                    }
                )
        provider <- providerFromConfig providerConfig
        let secret = encodeUtf8 "mysecret"
            secretHash = sha256 secret
        currentBlockHeight <- runBitcoinQueryMonadIO provider blockCount
        let lockedUntil = currentBlockHeight + 1
        step $ "currentBlockHeight: " <> show currentBlockHeight <> " lockedUntil: " <> show lockedUntil
        withContext $ \ctx -> do
          let testWallet2PublicKey = testWalletXPubKey2.key & wrapPubKey True
              testWalletPublicKey = testWalletXPubKey.key & wrapPubKey True
          (fundTx, fundTxSelectIns, htlcScript, htlcScriptOutputAddress) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ fundHTLC (btcToSatoshi 10) ctx secretHash testWallet2PublicKey testWalletPublicKey lockedUntil (Just 2) -- Create two outputs, one for testing redeem and other for the refund.
          (fundSignedTx, fundTxId) <- runBitcoinSignerMonadIO provider [testWalletAddress] testWalletAddress [testWalletXPrvKey.key] $ signAndSubmitFundHTLC (fundTx, fundTxSelectIns)
          step $ "fundSignedTx: " <> show fundSignedTx
          step $ "fundTxId: " <> show fundTxId
          threadDelay 20_000_000 -- wait for 20 seconds so that this transaction is mined.
          scriptUTxOs <- runBitcoinQueryMonadIO provider $ utxosAtAddress htlcScriptOutputAddress
          step $ "scriptUTxOs: " <> show scriptUTxOs
          let scriptUTxORedeem = head scriptUTxOs
              scriptUTxORefund = scriptUTxOs !! 1
          (redeemTx, _redeemTxSelectIns, sigHashRedeem) <- runBitcoinBuilderMonadIO provider [testWalletAddress2] testWalletAddress2 $ redeemHTLC scriptUTxORedeem htlcScript
          (redeemSignedTx, redeemTxId) <- runBitcoinQueryMonadIO provider $ signAndSubmitRedeemHTLC ctx testWalletXPrvKey2.key sigHashRedeem secret htlcScript redeemTx
          step $ "redeemSignedTx: " <> show redeemSignedTx
          step $ "redeemTxId: " <> show redeemTxId
          (refundTx, _refundTxSelectIns, sigHashRefund) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ refundHTLC scriptUTxORefund htlcScript lockedUntil
          (refundSignedTx, refundTxId) <- runBitcoinQueryMonadIO provider $ signAndSubmitRefundHTLC ctx testWalletXPrvKey.key sigHashRefund htlcScript refundTx
          step $ "refundSignedTx: " <> show refundSignedTx
          step $ "refundTxId: " <> show refundTxId
    ]