module ZkFold.Bitcoin.Types.Internal.HexByteString (
  HexByteString (unHexByteString),
  mkHexByteString,
  unsafeMkHexByteString,
  isHexByteString,
  hexByteStringToBytes,
  hexByteStringFromBytes,
  flipEndianness,
  hexByteStringToNatural,
) where

import Control.Arrow ((>>>))
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.Char (isHexDigit)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Natural (Natural)

{- | A valid hex byte string.

Example, "66ff", "0123ff", but not "012" (since it's of odd length) and also not "0g" since 'g' is not a valid hex character.
-}
newtype HexByteString = HexByteString {unHexByteString :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

-- | Check if a text is a valid hex string.
isHexString :: Text -> Bool
isHexString = Text.all isHexDigit

-- | Check if a text is a valid hex byte string.
isHexByteString :: Text -> Bool
isHexByteString t = isHexString t && even (Text.length t)

{- | Make a `HexByteString` from a `Text`.

>>> mkHexByteString "66ff"
-}
mkHexByteString :: Text -> Maybe HexByteString
mkHexByteString t =
  if isHexByteString t then Just (HexByteString t) else Nothing

-- | INTERNAL USAGE ONLY. Coerce a `Text` to a `HexByteString`.
unsafeMkHexByteString :: Text -> HexByteString
unsafeMkHexByteString = coerce

-- | Convert a `HexByteString` to a `ByteString`.
hexByteStringToBytes :: HexByteString -> ByteString
hexByteStringToBytes (HexByteString t) = Text.encodeUtf8 t & BS16.decode & either (\s -> error $ "hexByteStringToBytes: absurd, " <> s) id

hexByteStringFromBytes :: ByteString -> HexByteString
hexByteStringFromBytes =
  BS16.encode
    >>> Text.decodeUtf8
    >>> unsafeMkHexByteString

-- | Flip the endianness of a `HexByteString`.
flipEndianness :: HexByteString -> HexByteString
flipEndianness (HexByteString t) =
  t
    & Text.chunksOf 2
    & reverse
    & mconcat
    & unsafeMkHexByteString

-- | Obtain the natural number corresponding to the hex byte string.
hexByteStringToNatural :: HexByteString -> Natural
hexByteStringToNatural hbs =
  hbs
    & unHexByteString
    & ("0x" <>)
    & Text.unpack
    & read