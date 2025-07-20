module ZkFold.Bitcoin.Types.Internal.Common (
  LowerFirst,
  OutputIx,
  Satoshi,
) where

import Data.Char (toLower)
import Data.Word (Word32, Word64)
import Deriving.Aeson

data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier "" = ""
  getStringModifier (c : cs) = toLower c : cs

-- | Output index of an output in a transaction.
type OutputIx = Word32

-- | Satoshi is the smallest unit of Bitcoin.
type Satoshi = Word64