module ZkFold.Bitcoin.Types.Internal.Skeleton (
  TxSkeleton (..),
  mustHaveOutput,
) where

import Haskoin (ScriptOutput)
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)

newtype TxSkeleton = TxSkeleton
  { txSkelOuts :: [(ScriptOutput, Satoshi)]
  }
  deriving stock (Show)

instance Semigroup TxSkeleton where
  x <> y = TxSkeleton{txSkelOuts = txSkelOuts x <> txSkelOuts y}

instance Monoid TxSkeleton where
  mempty = TxSkeleton{txSkelOuts = mempty}

mustHaveOutput :: (ScriptOutput, Satoshi) -> TxSkeleton
mustHaveOutput (output, amt) = mempty{txSkelOuts = [(output, amt)]}