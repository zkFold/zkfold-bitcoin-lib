module ZkFold.Bitcoin.Types.Internal.BlockHeader (
  BlockHeader (..),
  blockHeaderFromText,
) where

import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
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