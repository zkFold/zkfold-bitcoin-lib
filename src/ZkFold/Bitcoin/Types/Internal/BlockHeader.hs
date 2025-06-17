module ZkFold.Bitcoin.Types.Internal.BlockHeader (
  BlockHeader (..),
  blockHeaderFromText,
  extractBlockHash,
  extractBlockTime,
) where

import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Natural (Natural)
import ZkFold.Bitcoin.Types.Internal.BlockHash (BlockHash (BlockHash))
import ZkFold.Bitcoin.Types.Internal.Common (isHexString)

newtype BlockHeader = BlockHeader {unBlockHeader :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

instance IsString BlockHeader where
  fromString s = Text.pack s & blockHeaderFromText & fromMaybe (error $ "Invalid block header: " <> s)

blockHeaderFromText :: Text -> Maybe BlockHeader
blockHeaderFromText t =
  if Text.length t == 160 && isHexString t then Just (BlockHeader t) else Nothing

instance FromJSON BlockHeader where
  parseJSON = withText "BlockHeader" $ \t ->
    case blockHeaderFromText t of
      Just h -> pure h
      Nothing -> fail $ Text.unpack $ "Invalid block header: " <> t

{- | Extract the previous block hash from a block header.

Following is block header of block 1 (just after genesis block).
>>> extractBlockHash "010000006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000982051fd1e4ba744bbbe680e1fee14677ba1a3c3540bf7b1cdb606e857233e0e61bc6649ffff001d01e36299"
BlockHash {unBlockHash = "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"}
-}
extractBlockHash :: BlockHeader -> BlockHash
extractBlockHash (BlockHeader header) =
  let blockHashLittleEndian = Text.take 64 $ Text.drop 8 header
   in blockHashLittleEndian
        & Text.chunksOf 2
        & reverse
        & mconcat
        & BlockHash

{- | Extract the block time from a block header.

>>> extractBlockTime "010000006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000982051fd1e4ba744bbbe680e1fee14677ba1a3c3540bf7b1cdb606e857233e0e61bc6649ffff001d01e36299"
1231469665
-}
extractBlockTime :: BlockHeader -> Natural
extractBlockTime (BlockHeader header) =
  let blockTimeLittleEndian = Text.take 8 $ Text.drop 136 header
   in blockTimeLittleEndian
        & Text.chunksOf 2
        & reverse
        & mconcat
        & ("0x" <>)
        & Text.unpack
        & read
