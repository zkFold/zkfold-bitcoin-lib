module ZkFold.Bitcoin.Examples.HTLC (
  HTLC (..),
  mkHTLC,
  fundHTLC,
  fundHTLCWithRecoveryData,
  signAndSubmitFundHTLC,
  redeemHTLC,
  addSigAndSubmitRedeemHTLC,
  signAndSubmitRedeemHTLC,
  refundHTLC,
  addSigRefundHTLC,
  addSigRefundHTLCAlt,
  addSigAndSubmitRefundHTLC,
  addSigAndSubmitRefundHTLCAlt,
  signAndSubmitRefundHTLC,
  signAndSubmitRefundHTLCAlt,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (stimesMonoid)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Haskoin (Address, Ctx, Hash256, PublicKey, Script (..), ScriptOp (..), ScriptOutput (..), Tx (..), TxSignature (..), WitnessProgram (..), encodeTxSig, marshal, opPushData, outputAddress, sigHashAll, signHash, toP2WSH, toWitnessStack, txSigHashForkId, updateIndex)
import Haskoin qualified
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (..), BitcoinSignerMonad, network, signTx)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Skeleton
import ZkFold.Bitcoin.Utils (wordToScriptOp)

data HTLC = HTLC
  { htlcScript :: Script
  , htlcScriptOutput :: ScriptOutput
  , htlcAddress :: Address
  , htlcSerializedScript :: ByteString
  , htlcTimelock :: Word32
  }
  deriving stock (Show, Eq, Generic)

-- TODO: Give FromJSON and ToJSON instances.

mkHTLC :: Ctx -> Hash256 -> PublicKey -> PublicKey -> PublicKey -> Word32 -> HTLC
mkHTLC ctx secretHash recipientPub refundPub1 refundPub2 timelock =
  let htlcScript = buildHTLC ctx secretHash recipientPub refundPub1 refundPub2 timelock
      htlcScriptOutput = toP2WSH htlcScript -- toP2SH htlcScript
      htlcAddress = outputAddress ctx htlcScriptOutput & fromJust
      htlcSerializedScript = runPutS $ serialize htlcScript
      htlcTimelock = timelock
   in HTLC{..}

-- | Fund HTLC contract. Usually, we'll use browser wallet API to send funds to this address instead of this function.
fundHTLC :: (BitcoinBuilderMonad m) => HTLC -> Satoshi -> Maybe Natural -> m (Tx, [UTxO])
fundHTLC HTLC{..} sats numOuts = do
  (tx, selectIns) <- buildTx $ stimesMonoid (fromMaybe 1 numOuts) $ mustHaveOutput (htlcScriptOutput, sats)
  pure (tx, selectIns)

-- | Fund HTLC and also add a small OP_RETURN output with recovery data.
fundHTLCWithRecoveryData :: (BitcoinBuilderMonad m) => Ctx -> Hash256 -> PublicKey -> HTLC -> Satoshi -> Maybe Natural -> m (Tx, [UTxO])
fundHTLCWithRecoveryData ctx secretHash refundPub1 HTLC{..} sats numOuts = do
  let recoveryOutput = htlcRecoveryOutput ctx secretHash refundPub1
  (tx, selectIns) <-
    buildTx $
      stimesMonoid (fromMaybe 1 numOuts) (mustHaveOutput (htlcScriptOutput, sats))
        <> mustHaveOutput (recoveryOutput, 0)
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

refundHTLC :: (BitcoinBuilderMonad m) => UTxO -> HTLC -> m (Tx, [UTxO], Hash256)
refundHTLC refundUTxO HTLC{..} = do
  net <- network
  (unsignedTx, selectIns) <- buildTx (mustHaveInput refundUTxO <> invalidUntil htlcTimelock)
  let sigHashRefund = txSigHashForkId net unsignedTx htlcScript (utxoValue refundUTxO) 0 sigHashAll
  pure (unsignedTx, selectIns, sigHashRefund)

addSigRefundHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.Sig -> HTLC -> Tx -> m Tx
addSigRefundHTLC ctx sigRefund HTLC{..} refundTx = do
  net <- network
  let txSigRefund = TxSignature sigRefund sigHashAll
      sigBSRefund = encodeTxSig net ctx txSigRefund
      -- select refund key #1 with OP_TRUE (minimal push 0x01)
      witnessStackRefund = [sigBSRefund, "\x01", "", htlcSerializedScript]
      witnessDataRefund = updateIndex 0 (replicate 1 $ toWitnessStack net ctx EmptyWitnessProgram) (const witnessStackRefund)
      finalTx = case refundTx of
        Tx{..} -> Tx{witness = witnessDataRefund, ..}
  pure finalTx

-- | Add refund signature using alternative refund pubkey (second key).
addSigRefundHTLCAlt :: (BitcoinQueryMonad m) => Ctx -> Haskoin.Sig -> HTLC -> Tx -> m Tx
addSigRefundHTLCAlt ctx sigRefund HTLC{..} refundTx = do
  net <- network
  let txSigRefund = TxSignature sigRefund sigHashAll
      sigBSRefund = encodeTxSig net ctx txSigRefund
      -- select refund key #2 with OP_FALSE (empty push)
      witnessStackRefund = [sigBSRefund, "", "", htlcSerializedScript]
      witnessDataRefund = updateIndex 0 (replicate 1 $ toWitnessStack net ctx EmptyWitnessProgram) (const witnessStackRefund)
      finalTx = case refundTx of
        Tx{..} -> Tx{witness = witnessDataRefund, ..}
  pure finalTx

addSigAndSubmitRefundHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.Sig -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
addSigAndSubmitRefundHTLC ctx sigRefund htlc refundTx = do
  finalTx <- addSigRefundHTLC ctx sigRefund htlc refundTx
  txIdRefund <- submitTx finalTx
  pure (finalTx, txIdRefund)

addSigAndSubmitRefundHTLCAlt :: (BitcoinQueryMonad m) => Ctx -> Haskoin.Sig -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
addSigAndSubmitRefundHTLCAlt ctx sigRefund htlc refundTx = do
  finalTx <- addSigRefundHTLCAlt ctx sigRefund htlc refundTx
  txIdRefund <- submitTx finalTx
  pure (finalTx, txIdRefund)

signAndSubmitRefundHTLC :: (BitcoinQueryMonad m) => Ctx -> Haskoin.SecKey -> Hash256 -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
signAndSubmitRefundHTLC ctx skey sigHashRefund htlc refundTx = do
  let sigRefund = signHash ctx skey sigHashRefund
  addSigAndSubmitRefundHTLC ctx sigRefund htlc refundTx

signAndSubmitRefundHTLCAlt :: (BitcoinQueryMonad m) => Ctx -> Haskoin.SecKey -> Hash256 -> HTLC -> Tx -> m (Tx, Haskoin.TxHash)
signAndSubmitRefundHTLCAlt ctx skey sigHashRefund htlc refundTx = do
  let sigRefund = signHash ctx skey sigHashRefund
  addSigAndSubmitRefundHTLCAlt ctx sigRefund htlc refundTx

buildHTLC :: Ctx -> Hash256 -> PublicKey -> PublicKey -> PublicKey -> Word32 -> Script
buildHTLC ctx secretHash recipientPub refundPub1 refundPub2 timelock =
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
    , -- Select which refund key to verify with a boolean flag:
      -- OP_TRUE selects refundPub1; OP_FALSE selects refundPub2.
      OP_IF
    , opPushData (marshal ctx refundPub1)
    , OP_CHECKSIG
    , OP_ELSE
    , opPushData (marshal ctx refundPub2)
    , OP_CHECKSIG
    , OP_ENDIF
    , OP_ENDIF
    ]

htlcRecoveryTag :: ByteString
htlcRecoveryTag = "ZKHTLC1"

htlcRecoveryPayload :: Ctx -> Hash256 -> PublicKey -> ByteString
htlcRecoveryPayload ctx secretHash refundPub1 =
  let secretHashBS = runPutS $ serialize secretHash
      refundPub1BS = marshal ctx refundPub1
   in BS.concat [htlcRecoveryTag, secretHashBS, refundPub1BS]

htlcRecoveryOutput :: Ctx -> Hash256 -> PublicKey -> ScriptOutput
htlcRecoveryOutput ctx secretHash refundPub1 =
  DataCarrier (htlcRecoveryPayload ctx secretHash refundPub1)