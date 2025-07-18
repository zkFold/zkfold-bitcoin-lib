module ZkFold.Bitcoin.Types.Internal.UTxO (
  UTxO (..),
) where

import Data.Word (Word64)
import Haskoin (OutPoint)

data UTxO = UTxO
  { utxoOutpoint :: OutPoint
  , utxoValue :: Word64
  }
  deriving stock (Show)