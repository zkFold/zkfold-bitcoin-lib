module ZkFold.Bitcoin.Types.Internal.Common (
  LowerFirst,
  OutputIx,
  Satoshi,
  Bitcoin,
  btcToSatoshi,
  btcFromSatoshi,
  FeePerByte,
) where

import Data.Char (toLower)
import Data.Fixed (Fixed (..), HasResolution (..))
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

data E8

instance HasResolution E8 where
  resolution _ = 100_000_000

{- | 100_000_000 satoshis make one bitcoin.

>>> btcFromSatoshi 123_456_789
1.23456789
-}
type Bitcoin = Fixed E8

btcToSatoshi :: Bitcoin -> Satoshi
btcToSatoshi (MkFixed x) = fromIntegral x

btcFromSatoshi :: Satoshi -> Bitcoin
btcFromSatoshi x = MkFixed (fromIntegral x)

type FeePerByte = Satoshi