module ZkFold.Bitcoin.Types.Internal.UTxO (
  UTxO (..),
) where

import Haskoin (Address, Coin (..), OutPoint)
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)

data UTxO = UTxO
  { utxoOutpoint :: OutPoint
  , utxoValue :: Satoshi
  , utxoAddress :: Address
  }
  deriving stock (Show)

instance Coin UTxO where
  coinValue = utxoValue