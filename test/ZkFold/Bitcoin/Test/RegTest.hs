{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Base16 qualified as BS16
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Semigroup (stimesMonoid)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import GHC.Natural (Natural)
import Haskoin (Ctx, Hash256, PublicKey, Script (..), ScriptOp (..), Tx (..), TxSignature (..), WitnessProgram (..), encodeTxSig, marshal, opPushData, outputAddress, sha256, sigHashAll, signHash, toP2WSH, toWitnessStack, txSigHashForkId, updateIndex, withContext, wrapPubKey)
import Haskoin qualified
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (..), signTx)
import ZkFold.Bitcoin.IO
import ZkFold.Bitcoin.Test.Constants (testWalletAddress, testWalletAddress2, testWalletXPrvKey, testWalletXPrvKey2, testWalletXPubKey, testWalletXPubKey2)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (btcToSatoshi)
import ZkFold.Bitcoin.Types.Internal.Skeleton
import ZkFold.Bitcoin.Utils (wordToScriptOp)

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
        testWalletUTxOs <- runBitcoinQueryMonadIO provider $ do
          utxosAtAddress testWalletAddress
        step $ "testWalletUTxOs: " <> show testWalletUTxOs
        let secret = encodeUtf8 "mysecret"
            secretHash = sha256 secret
        currentBlockHeight <- runBitcoinQueryMonadIO provider blockCount
        let lockedUntil = currentBlockHeight + 1
        step $ "currentBlockHeight: " <> show currentBlockHeight
        nid <- runBitcoinQueryMonadIO provider networkId
        let network = networkFromId nid
        withContext $ \ctx -> do
          let testWallet2PublicKey = testWalletXPubKey2.key & wrapPubKey True
              testWalletPublicKey = testWalletXPubKey.key & wrapPubKey True
              htlcScript = buildHTLC ctx secretHash testWallet2PublicKey testWalletPublicKey lockedUntil
              htlcScriptOutput = toP2WSH htlcScript -- toP2SH htlcScript
              htlcScriptOutputAddress = outputAddress ctx htlcScriptOutput & fromJust
              -- Create two outputs, one for testing redeem and other for the refund.
              txSkel = stimesMonoid (2 :: Natural) $ mustHaveOutput (htlcScriptOutput, btcToSatoshi 10)
          (tx, selectIns) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ buildTx txSkel
          step $ "tx: " <> show tx
          signedTx <- runBitcoinSignerMonadIO provider [testWalletAddress] testWalletAddress [testWalletXPrvKey.key] $ signTx (tx, selectIns)
          step $ "signedTx: " <> show signedTx
          txid <- runBitcoinQueryMonadIO provider $ submitTx signedTx
          step $ "txid: " <> show txid
          threadDelay 20_000_000 -- wait for 20 seconds so that this transaction is mined.
          scriptUTxOs <- runBitcoinQueryMonadIO provider $ utxosAtAddress htlcScriptOutputAddress
          step $ "scriptUTxOs: " <> show scriptUTxOs
          let scriptUTxORedeem = head scriptUTxOs
              scriptUTxORefund = scriptUTxOs !! 1
          (redeemTx, _) <- runBitcoinBuilderMonadIO provider [testWalletAddress2] testWalletAddress2 $ buildTx (mustHaveInput scriptUTxORedeem)
          let sigHashRedeem = txSigHashForkId network redeemTx htlcScript (utxoValue scriptUTxORedeem) 0 sigHashAll -- txSigHash for P2SH.
              sigRedeem = signHash ctx testWalletXPrvKey2.key sigHashRedeem
              txSigRedeem = TxSignature sigRedeem sigHashAll
              sigBSRedeem = encodeTxSig network ctx txSigRedeem
              redeemBS = runPutS $ serialize htlcScript
              redeemBSHex = BS16.encode redeemBS
              witnessStack = [sigBSRedeem, secret, "\x01", redeemBS] -- In SegWit (by standardness) and Taproot (by consensus), the arguments to OP_IF and OP_NOTIF must be minimal. See https://bitcoin.stackexchange.com/a/122826 for more details.
              witnessData = updateIndex 0 (replicate 1 $ toWitnessStack network ctx EmptyWitnessProgram) (const witnessStack)
              -- inputScript =
              --   Script
              --     [ opPushData sigBSRedeem
              --     , opPushData secret
              --     , OP_1
              --     , opPushData redeemBS
              --     ]
              -- updatedInputs =
              --   updateIndex
              --     0
              --     redeemTx.inputs
              --     ( \TxIn{..} ->
              --         TxIn
              --           { outpoint
              --           , sequence
              --           , script = runPutS $ serialize inputScript
              --           }
              --     )
              finalTx = case redeemTx of
                Tx{..} -> Tx{witness = witnessData, ..}
          -- inputs = updatedInputs for P2SH
          step $ "redeemBSHex: " <> show redeemBSHex
          step $ "finalTx: " <> show finalTx
          txIdRedeem <- runBitcoinQueryMonadIO provider $ submitTx finalTx
          step $ "txIdRedeem: " <> show txIdRedeem
          (refundTx, _) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ buildTx (mustHaveInput scriptUTxORefund <> invalidUntil lockedUntil)
          let sigHashRefund = txSigHashForkId network refundTx htlcScript (utxoValue scriptUTxORefund) 0 sigHashAll -- txSigHash for P2SH.
              sigRefund = signHash ctx testWalletXPrvKey.key sigHashRefund
              txSigRefund = TxSignature sigRefund sigHashAll
              sigBSRefund = encodeTxSig network ctx txSigRefund
              witnessStackRefund = [sigBSRefund, "", redeemBS]
              witnessDataRefund = updateIndex 0 (replicate 1 $ toWitnessStack network ctx EmptyWitnessProgram) (const witnessStackRefund)
              finalTxRefund = case refundTx of
                Tx{..} -> Tx{witness = witnessDataRefund, ..}
          step $ "finalTxRefund: " <> show finalTxRefund
          step $ "finalTxRefundHex: " <> show (BS16.encode $ runPutS $ serialize finalTxRefund)
          latestBlockHeight <- runBitcoinQueryMonadIO provider blockCount
          step $ "latestBlockHeight: " <> show latestBlockHeight
          txIdRefund <- runBitcoinQueryMonadIO provider $ submitTx finalTxRefund
          step $ "txIdRefund: " <> show txIdRefund
    ]

buildHTLC :: Ctx -> Hash256 -> PublicKey -> PublicKey -> Word32 -> Script
buildHTLC ctx secretHash recipientPub refundPub timelock =
  Script
    [ OP_IF
    , OP_SHA256
    , opPushData (runPutS $ serialize secretHash) -- Push secret hash (32 bytes)
    , OP_EQUALVERIFY
    , opPushData (marshal ctx recipientPub) -- Push recipient pubkey (33 bytes compressed)
    , OP_CHECKSIG
    , OP_ELSE
    , wordToScriptOp timelock
    , OP_CHECKLOCKTIMEVERIFY
    , OP_DROP
    , opPushData (marshal ctx refundPub) -- Push refund pubkey (33 bytes compressed)
    , OP_CHECKSIG
    , OP_ENDIF
    ]