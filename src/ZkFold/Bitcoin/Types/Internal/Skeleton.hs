module ZkFold.Bitcoin.Types.Internal.Skeleton (
  TxSkeleton (..),
  mustHaveInput,
  mustHaveOutput,
  invalidUntil,
  buildTxFromSkeleton,
) where

import Data.ByteString qualified as BS
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Haskoin (ScriptOutput)
import Haskoin qualified
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)
import ZkFold.Bitcoin.Types.Internal.UTxO (UTxO (..))

data TxSkeleton = TxSkeleton
  { txSkelIns :: [UTxO] -- TODO: Make it a set since we don't enforce any ordering?
  , txSkelOuts :: [(ScriptOutput, Satoshi)]
  , txSkelLocktime :: Maybe Word32
  -- ^ The locktime of the transaction. If not provided, the transaction will be valid immediately. You can then set the locktime field to between 0 and 499999999 for the transaction to be able to be mined after a certain block height, or between 500000000 and 4294967295 for it to be mined after a specific point in time (i.e. a Unix timestamp). When combining two skeletons, the locktime of the resulting skeleton will be the maximum of the two locktimes.
  -- TODO: Abstract locktime to a more general type which allows for safety in relation whether it represents a block height or a timestamp.
  }
  deriving stock (Show)

instance Semigroup TxSkeleton where
  x <> y =
    TxSkeleton
      { txSkelIns = nubBy ((==) `on` utxoOutpoint) (txSkelIns x <> txSkelIns y)
      , txSkelOuts = txSkelOuts x <> txSkelOuts y
      , txSkelLocktime = combineLocktimes (txSkelLocktime x) (txSkelLocktime y)
      }
   where
    combineLocktimes xlt Nothing = xlt
    combineLocktimes Nothing ylt = ylt
    combineLocktimes (Just xlt) (Just ylt) = Just $ max xlt ylt

instance Monoid TxSkeleton where
  mempty =
    TxSkeleton
      { txSkelIns = mempty
      , txSkelOuts = mempty
      , txSkelLocktime = Nothing
      }

-- | A transaction which must have a certain input.
mustHaveInput :: UTxO -> TxSkeleton
mustHaveInput utxo = mempty{txSkelIns = [utxo]}

-- | A transaction which must have a certain output.
mustHaveOutput :: (ScriptOutput, Satoshi) -> TxSkeleton
mustHaveOutput (output, amt) = mempty{txSkelOuts = [(output, amt)]}

-- | A transaction which is invalid until a certain locktime.
invalidUntil :: Word32 -> TxSkeleton
invalidUntil locktime = mempty{txSkelLocktime = Just locktime}

buildTxFromSkeleton ::
  Haskoin.Ctx ->
  TxSkeleton ->
  Haskoin.Tx
buildTxFromSkeleton ctx TxSkeleton{..} =
  Haskoin.Tx 2 (toIn . utxoOutpoint <$> txSkelIns) (toOut <$> txSkelOuts) [] (fromMaybe 0 txSkelLocktime)
 where
  toIn op =
    Haskoin.TxIn
      op
      BS.empty
      -- Allows for locktimes. This same value is also used by Bitcoin core by default.
      -- TODO: Allow it to be intelligently inferred from the skeleton? Eventually, we'd like to support RBF and relative timelocks.
      0xFFFFFFFE
  toOut (o, v) = Haskoin.TxOut v $ Haskoin.marshal ctx o