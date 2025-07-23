module ZkFold.Bitcoin.Types.Internal.UTxO (
  UTxO (..),
) where

import Haskoin (Coin (..), OutPoint)
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)

data UTxO = UTxO
  { utxoOutpoint :: OutPoint
  , utxoValue :: Satoshi
  }
  deriving stock (Show)

instance Coin UTxO where
  coinValue = utxoValue