{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Haskoin (Ctx, Hash256, PublicKey, Script (..), ScriptOp (..), Tx (..), TxIn (..), TxSignature (..), addressToOutput, encodeTxSig, marshal, opPushData, outputAddress, payToScriptAddress, script, sha256, sigHashAll, signHash, toP2SH, txSigHash, updateIndex, withContext, wrapPubKey)
import Haskoin qualified
import Haskoin.Crypto.Keys.Extended qualified as H
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (..), signTx)
import ZkFold.Bitcoin.IO
import ZkFold.Bitcoin.Test.Constants (testWalletAddress, testWalletAddress2, testWalletXPrvKey, testWalletXPrvKey2, testWalletXPubKey, testWalletXPubKey2)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (btcToSatoshi)
import ZkFold.Bitcoin.Types.Internal.Skeleton

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
        currentBlockHeight <- runBitcoinQueryMonadIO provider $ blockCount
        step $ "currentBlockHeight: " <> show currentBlockHeight
        nid <- runBitcoinQueryMonadIO provider $ networkId
        let network = networkFromId nid
        withContext $ \ctx -> do
          let testWallet2PublicKey = testWalletXPubKey2.key & wrapPubKey True
              testWalletPublicKey = testWalletXPubKey.key & wrapPubKey True
              htlcScript = buildHTLCRedeem ctx secretHash testWallet2PublicKey testWalletPublicKey (currentBlockHeight + 4) -- locked till 4 blocks
              htlcScriptOutput = toP2SH htlcScript
              htlcScriptOutputAddress = outputAddress ctx htlcScriptOutput & fromJust

          let txSkel = mustHaveOutput (htlcScriptOutput, btcToSatoshi 10)
          (tx, selectIns) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ buildTx txSkel
          step $ "tx: " <> show tx
          signedTx <- runBitcoinSignerMonadIO provider [testWalletAddress] testWalletAddress [testWalletXPrvKey.key] $ signTx (tx, selectIns)
          step $ "signedTx: " <> show signedTx
          txid <- runBitcoinQueryMonadIO provider $ submitTx signedTx
          step $ "txid: " <> show txid
          threadDelay 20000000 -- wait for 20 seconds
          scriptUTxOs <- runBitcoinQueryMonadIO provider $ utxosAtAddress htlcScriptOutputAddress
          step $ "scriptUTxOs: " <> show scriptUTxOs
          let scriptUTxO = head scriptUTxOs
          -- TODO: Appropriate fee estimation.
          let redeemTx = Haskoin.buildTx ctx [scriptUTxO & utxoOutpoint] [(testWalletAddress2 & addressToOutput, btcToSatoshi 10 - 1000)]
              sigHash = txSigHash network redeemTx htlcScript (utxoValue scriptUTxO) 0 sigHashAll
              sig = signHash ctx (testWalletXPrvKey2.key) sigHash
              txSig = TxSignature sig sigHashAll
              sigBS = encodeTxSig network ctx txSig
              redeemBS = runPutS $ serialize htlcScript
              inputScript =
                Script
                  [ opPushData sigBS
                  , opPushData secret
                  , OP_1
                  , opPushData redeemBS
                  ]
              updatedInputs =
                updateIndex
                  0
                  redeemTx.inputs
                  ( \TxIn{..} ->
                      TxIn
                        { outpoint
                        , sequence
                        , script = runPutS $ serialize inputScript
                        }
                  )
              finalTx = case redeemTx of
                Tx{..} -> Tx{version, outputs, witness, locktime, inputs = updatedInputs}
          step $ "finalTx: " <> show finalTx
          txIdRedeem <- runBitcoinQueryMonadIO provider $ submitTx finalTx
          step $ "txIdRedeem: " <> show txIdRedeem
    ]

buildHTLCRedeem :: Ctx -> Hash256 -> PublicKey -> PublicKey -> Word32 -> Script
buildHTLCRedeem ctx secretHash recipientPub refundPub timelock =
  Script
    [ OP_IF
    , OP_SHA256
    , opPushData (runPutS $ serialize secretHash) -- Push secret hash (32 bytes)
    , OP_EQUALVERIFY
    , opPushData (marshal ctx recipientPub) -- Push recipient pubkey (33 bytes compressed)
    , OP_CHECKSIG
    , OP_ELSE
    , opPushData (runPutS $ serialize timelock) -- Push timelock as int (typically 4-5 bytes)
    , OP_CHECKLOCKTIMEVERIFY
    , OP_DROP
    , opPushData (marshal ctx refundPub) -- Push refund pubkey (33 bytes compressed)
    , OP_CHECKSIG
    , OP_ENDIF
    ]