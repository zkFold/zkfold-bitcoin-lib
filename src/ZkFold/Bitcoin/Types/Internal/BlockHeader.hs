module ZkFold.Bitcoin.Types.Internal.BlockHeader (
  BlockHeader (..),
  blockHeaderFromText,
  extractPreviousBlockHash,
  extractBlockTime,
  obtainBlockHash,
) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Natural (Natural)
import ZkFold.Bitcoin.Types.Internal.BlockHash (BlockHash (BlockHash))
import ZkFold.Bitcoin.Types.Internal.HexByteString (HexByteString (..), flipEndianness, hexByteStringFromBytes, hexByteStringToBytes, mkHexByteString, unsafeMkHexByteString)

-- | Block header, in hex format.
newtype BlockHeader = BlockHeader {unBlockHeader :: HexByteString}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

instance IsString BlockHeader where
  fromString s = Text.pack s & blockHeaderFromText & fromMaybe (error $ "Invalid block header: " <> s)

blockHeaderFromText :: Text -> Maybe BlockHeader
blockHeaderFromText t =
  mkHexByteString t >>= \hbs -> if Text.length t == 160 then Just (BlockHeader hbs) else Nothing

instance FromJSON BlockHeader where
  parseJSON = withText "BlockHeader" $ \t ->
    case blockHeaderFromText t of
      Just h -> pure h
      Nothing -> fail $ Text.unpack $ "Invalid block header: " <> t

{- | Extract the previous block hash from a block header.

Following is block header of block 1 (just after genesis block).
>>> extractPreviousBlockHash "010000006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000982051fd1e4ba744bbbe680e1fee14677ba1a3c3540bf7b1cdb606e857233e0e61bc6649ffff001d01e36299"
BlockHash {unBlockHash = HexByteString {unHexByteString = "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"}}
-}
extractPreviousBlockHash :: BlockHeader -> BlockHash
extractPreviousBlockHash (BlockHeader (unHexByteString -> header)) =
  let blockHashLittleEndian = unsafeMkHexByteString $ Text.take 64 $ Text.drop 8 header
   in blockHashLittleEndian
        & flipEndianness
        & BlockHash

{- | Extract the block time from a block header.

>>> extractBlockTime "010000006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000982051fd1e4ba744bbbe680e1fee14677ba1a3c3540bf7b1cdb606e857233e0e61bc6649ffff001d01e36299"
1231469665
-}
extractBlockTime :: BlockHeader -> Natural
extractBlockTime (BlockHeader (unHexByteString -> header)) =
  let blockTimeLittleEndian = unsafeMkHexByteString $ Text.take 8 $ Text.drop 136 header
   in blockTimeLittleEndian
        & flipEndianness
        & unHexByteString
        & ("0x" <>)
        & Text.unpack
        & read

{- | Obtain the block's hash from it's header.

>>> obtainBlockHash "010000006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000982051fd1e4ba744bbbe680e1fee14677ba1a3c3540bf7b1cdb606e857233e0e61bc6649ffff001d01e36299"
BlockHash {unBlockHash = HexByteString {unHexByteString = "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048"}}
-}
obtainBlockHash :: BlockHeader -> BlockHash
obtainBlockHash (BlockHeader header) =
  let headerBS = hexByteStringToBytes header
   in headerBS
        & SHA256.hash
        & SHA256.hash
        & hexByteStringFromBytes
        & flipEndianness
        & BlockHash
