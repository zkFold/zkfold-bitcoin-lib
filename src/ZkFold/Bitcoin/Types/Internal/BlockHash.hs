module ZkFold.Bitcoin.Types.Internal.BlockHash (
  BlockHash (..),
  blockHashFromText,
) where

import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import ZkFold.Bitcoin.Types.Internal.Common (isHexString)

newtype BlockHash = BlockHash {unBlockHash :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

instance IsString BlockHash where
  fromString s = Text.pack s & blockHashFromText & fromMaybe (error $ "Invalid block hash: " <> s)

blockHashFromText :: Text -> Maybe BlockHash
blockHashFromText t =
  if Text.length t == 64 && isHexString t then Just (BlockHash t) else Nothing

instance FromJSON BlockHash where
  parseJSON = withText "BlockHash" $ \t ->
    case blockHashFromText t of
      Just h -> pure h
      Nothing -> fail $ Text.unpack $ "Invalid block hash: " <> t