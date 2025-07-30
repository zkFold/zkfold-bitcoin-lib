module ZkFold.Bitcoin.Examples.HTLC (
  HTLC (..),
  mkHTLC,
  fundHTLC,
  signAndSubmitFundHTLC,
  redeemHTLC,
  addSigAndSubmitRedeemHTLC,
  signAndSubmitRedeemHTLC,
  refundHTLC,
  addSigAndSubmitRefundHTLC,
  signAndSubmitRefundHTLC,
) where

import Data.ByteString (ByteString)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (stimesMonoid)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Haskoin (Address, Ctx, Hash256, PublicKey, Script (..), ScriptOp (..), ScriptOutput, Tx (..), TxSignature (..), WitnessProgram (..), encodeTxSig, marshal, opPushData, outputAddress, sigHashAll, signHash, toP2WSH, toWitnessStack, txSigHashForkId, updateIndex)
import Haskoin qualified
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (..), BitcoinSignerMonad, network, signTx)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)
import ZkFold.Bitcoin.Types.Internal.Skeleton
import ZkFold.Bitcoin.Utils (wordToScriptOp)

data HTLC = HTLC
  { htlcScript :: Script
  , htlcScriptOutput :: ScriptOutput
  , htlcAddress :: Address
  , htlcSerializedScript :: ByteString
  }
  deriving stock (Show, Eq, Generic)

-- TODO: Give FromJSON and ToJSON instances.

mkHTLC :: Ctx -> Hash256 -> PublicKey -> PublicKey -> Word32 -> HTLC
mkHTLC ctx secretHash recipientPub refundPub timelock =
  let htlcScript = buildHTLC ctx secretHash recipientPub refundPub timelock
      htlcScriptOutput = toP2WSH htlcScript -- toP2SH htlcScript
      htlcAddress = outputAddress ctx htlcScriptOutput & fromJust
      htlcSerializedScript = runPutS $ serialize htlcScript
   in HTLC{..}

fundHTLC :: (BitcoinBuilderMonad m) => HTLC -> Satoshi -> Maybe Natural -> m (Tx, [UTxO])
fundHTLC HTLC{..} sats numOuts = do
  (tx, selectIns) <- buildTx $ stimesMonoid (fromMaybe 1 numOuts) $ mustHaveOutput (htlcScriptOutput, sats)
  -- TODO: Should also return sigHash for easy signing?
  pure (tx, selectIns)

signAndSubmitFundHTLC :: (BitcoinSignerMonad m) => (Tx, [UTxO]) -> m (Tx, Haskoin.TxHash)
signAndSubmitFundHTLC (tx, selectIns) = do
  signedTx <- signTx (tx, selectIns)
  tid <- submitTx signedTx
  pure (signedTx, tid)

redeemHTLC :: (BitcoinBuilderMonad m) => UTxO -> HTLC -> m (Tx, [UTxO], Hash256)
redeemHTLC redeemUTxO HTLC{..} = do
  (unsignedTx, selectIns) <- buildTx (mustHaveInput redeemUTxO)
  net <- network
  let sigHashRedeem = txSigHashForkId net unsignedTx htlcScript (utxoValue redeemUTxO) 0 sigHashAll -- txSigHash for P2SH.
  pure (unsignedTx, selectIns, sigHashRedeem)

addSigAndSubmitRedeemHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.Sig -> ByteString -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
addSigAndSubmitRedeemHTLC ctx sigRedeem secret HTLC{..} redeemTx = do
  net <- network
  let txSigRedeem = TxSignature sigRedeem sigHashAll
      sigBSRedeem = encodeTxSig net ctx txSigRedeem
      witnessStack = [sigBSRedeem, secret, "\x01", htlcSerializedScript] -- In SegWit (by standardness) and Taproot (by consensus), the arguments to OP_IF and OP_NOTIF must be minimal. See https://bitcoin.stackexchange.com/a/122826 for more details.
      witnessData = updateIndex 0 (replicate 1 $ toWitnessStack net ctx EmptyWitnessProgram) (const witnessStack)
      -- inputScript =
      --   Script
      --     [ opPushData sigBSRedeem
      --     , opPushData secret
      --     , OP_1
      --     , opPushData htlcSerializedScript
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
  txIdRedeem <- submitTx finalTx
  pure (finalTx, txIdRedeem)

signAndSubmitRedeemHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.SecKey -> Hash256 -> ByteString -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
signAndSubmitRedeemHTLC ctx skey sigHashRedeem secret htlc redeemTx = do
  let sigRedeem = signHash ctx skey sigHashRedeem
  addSigAndSubmitRedeemHTLC ctx sigRedeem secret htlc redeemTx

refundHTLC :: (BitcoinBuilderMonad m) => UTxO -> HTLC -> Word32 -> m (Tx, [UTxO], Hash256)
refundHTLC refundUTxO HTLC{..} lockedUntil = do
  net <- network
  (unsignedTx, selectIns) <- buildTx (mustHaveInput refundUTxO <> invalidUntil lockedUntil)
  let sigHashRefund = txSigHashForkId net unsignedTx htlcScript (utxoValue refundUTxO) 0 sigHashAll
  pure (unsignedTx, selectIns, sigHashRefund)

addSigAndSubmitRefundHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.Sig -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
addSigAndSubmitRefundHTLC ctx sigRefund HTLC{..} refundTx = do
  net <- network
  let txSigRefund = TxSignature sigRefund sigHashAll
      sigBSRefund = encodeTxSig net ctx txSigRefund
      witnessStackRefund = [sigBSRefund, "", htlcSerializedScript]
      witnessDataRefund = updateIndex 0 (replicate 1 $ toWitnessStack net ctx EmptyWitnessProgram) (const witnessStackRefund)
      finalTx = case refundTx of
        Tx{..} -> Tx{witness = witnessDataRefund, ..}
  txIdRefund <- submitTx finalTx
  pure (finalTx, txIdRefund)

signAndSubmitRefundHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.SecKey -> Hash256 -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
signAndSubmitRefundHTLC ctx skey sigHashRefund htlc refundTx = do
  let sigRefund = signHash ctx skey sigHashRefund
  addSigAndSubmitRefundHTLC ctx sigRefund htlc refundTx

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