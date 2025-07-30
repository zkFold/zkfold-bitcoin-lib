module ZkFold.Bitcoin.Examples.HTLC (
  fundHTLC,
  signAndSubmitFundHTLC,
  redeemHTLC,
  addSigAndSubmitRedeemHTLC,
  signAndSubmitRedeemHTLC,
  refundHTLC,
  addSigAndSubmitRefundHTLC,
  signAndSubmitRefundHTLC,
) where

import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Function ((&))
import Data.Maybe ( fromJust, fromMaybe )
import Data.Semigroup (stimesMonoid)
import Data.Word (Word32)
import GHC.Natural (Natural)
import Haskoin (Ctx, Hash256, PublicKey, Script (..), ScriptOp (..), Tx (..), TxSignature (..), WitnessProgram (..), encodeTxSig, marshal, opPushData, outputAddress, sigHashAll, signHash, toP2WSH, toWitnessStack, txSigHashForkId, updateIndex, Address)
import Haskoin qualified
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (..), signTx, BitcoinSignerMonad, network)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)
import ZkFold.Bitcoin.Types.Internal.Skeleton
import ZkFold.Bitcoin.Utils (wordToScriptOp)
import Data.ByteString (ByteString)

fundHTLC :: BitcoinBuilderMonad m => Satoshi -> Ctx -> Hash256 -> PublicKey -> PublicKey -> Word32 -> Maybe Natural -> m (Tx, [UTxO], Script, Address)
fundHTLC sats ctx secretHash recipientPub refundPub timelock numOuts = do
  let htlcScript = buildHTLC ctx secretHash recipientPub refundPub timelock
      htlcScriptOutput = toP2WSH htlcScript -- toP2SH htlcScript
      htlcScriptOutputAddress = outputAddress ctx htlcScriptOutput & fromJust
      txSkel = stimesMonoid (fromMaybe 1 numOuts) $ mustHaveOutput (htlcScriptOutput, sats)
  (tx, selectIns) <- buildTx txSkel
  -- TODO: Should also return sigHash for easy signing?
  pure (tx, selectIns, htlcScript, htlcScriptOutputAddress)

signAndSubmitFundHTLC :: BitcoinSignerMonad m => (Tx, [UTxO]) -> m (Tx, Haskoin.TxHash)
signAndSubmitFundHTLC (tx, selectIns) = do
  signedTx <- signTx (tx, selectIns)
  tid <- submitTx signedTx
  pure (signedTx, tid)

redeemHTLC :: BitcoinBuilderMonad m => UTxO -> Script -> m (Tx, [UTxO], Hash256)
redeemHTLC redeemUTxO htlcScript = do
  (unsignedTx, selectIns) <- buildTx (mustHaveInput redeemUTxO)
  net <- network
  let sigHashRedeem = txSigHashForkId net unsignedTx htlcScript (utxoValue redeemUTxO) 0 sigHashAll -- txSigHash for P2SH.
  pure (unsignedTx, selectIns, sigHashRedeem)

addSigAndSubmitRedeemHTLC :: (BitcoinQueryMonad m, Serial p) => Ctx -> Haskoin.Sig -> ByteString -> p -> Tx -> m (Tx, Haskoin.TxHash)
addSigAndSubmitRedeemHTLC ctx sigRedeem secret htlcScript redeemTx = do
  net <- network
  let txSigRedeem = TxSignature sigRedeem sigHashAll
      sigBSRedeem = encodeTxSig net ctx txSigRedeem
      redeemBS = runPutS $ serialize htlcScript
      witnessStack = [sigBSRedeem, secret, "\x01", redeemBS] -- In SegWit (by standardness) and Taproot (by consensus), the arguments to OP_IF and OP_NOTIF must be minimal. See https://bitcoin.stackexchange.com/a/122826 for more details.
      witnessData = updateIndex 0 (replicate 1 $ toWitnessStack net ctx EmptyWitnessProgram) (const witnessStack)
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
        Tx{..} -> Tx{witness = witnessData, ..} -- inputs = updatedInputs for P2SH
  txIdRedeem <- submitTx finalTx
  pure (finalTx, txIdRedeem)


signAndSubmitRedeemHTLC :: (BitcoinQueryMonad m, Serial p) => Ctx -> Haskoin.SecKey -> Hash256 -> ByteString -> p -> Tx -> m (Tx, Haskoin.TxHash)
signAndSubmitRedeemHTLC ctx skey sigHashRedeem secret htlcScript redeemTx = do
  let sigRedeem = signHash ctx skey sigHashRedeem
  addSigAndSubmitRedeemHTLC ctx sigRedeem secret htlcScript redeemTx

-- TODO: A type for usual script details that we can pass on instead of passing separately @htlcScript@ or @lockedUntil@ or @redeemBS@.
refundHTLC :: BitcoinBuilderMonad m => UTxO -> Script -> Word32 -> m (Tx, [UTxO], Hash256)
refundHTLC refundUTxO htlcScript lockedUntil = do
  net <- network
  (unsignedTx, selectIns) <- buildTx (mustHaveInput refundUTxO <> invalidUntil lockedUntil)
  let sigHashRefund = txSigHashForkId net unsignedTx htlcScript (utxoValue refundUTxO) 0 sigHashAll
  pure (unsignedTx, selectIns, sigHashRefund)

addSigAndSubmitRefundHTLC :: (BitcoinQueryMonad m, Serial p) => Ctx -> Haskoin.Sig -> p -> Tx -> m (Tx, Haskoin.TxHash)
addSigAndSubmitRefundHTLC ctx sigRefund htlcScript refundTx = do
  net <- network
  let txSigRefund = TxSignature sigRefund sigHashAll
      sigBSRefund = encodeTxSig net ctx txSigRefund
      redeemBS = runPutS $ serialize htlcScript
      witnessStackRefund = [sigBSRefund, "", redeemBS]
      witnessDataRefund = updateIndex 0 (replicate 1 $ toWitnessStack net ctx EmptyWitnessProgram) (const witnessStackRefund)
      finalTx = case refundTx of
        Tx{..} -> Tx{witness = witnessDataRefund, ..}
  txIdRefund <- submitTx finalTx
  pure (finalTx, txIdRefund)

signAndSubmitRefundHTLC :: (BitcoinQueryMonad m, Serial p) => Ctx -> Haskoin.SecKey -> Hash256 -> p -> Tx -> m (Tx, Haskoin.TxHash)
signAndSubmitRefundHTLC ctx skey sigHashRefund htlcScript refundTx = do
  let sigRefund = signHash ctx skey sigHashRefund
  addSigAndSubmitRefundHTLC ctx sigRefund htlcScript refundTx

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