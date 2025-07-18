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
import Servant.API (ToHttpApiData (..))
import ZkFold.Bitcoin.Types.Internal.HexByteString

newtype BlockHash = BlockHash {unBlockHash :: HexByteString}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

instance IsString BlockHash where
  fromString s = Text.pack s & blockHashFromText & fromMaybe (error $ "Invalid block hash: " <> s)

blockHashFromText :: Text -> Maybe BlockHash
blockHashFromText t =
  mkHexByteString t >>= \hbs -> if Text.length t == 64 then Just (BlockHash hbs) else Nothing

instance Read BlockHash where
  readsPrec _ str =
    case blockHashFromText (Text.pack str) of
      Just bh -> [(bh, "")]
      Nothing -> []

instance ToHttpApiData BlockHash where
  toUrlPiece = unHexByteString . unBlockHash

instance FromJSON BlockHash where
  parseJSON = withText "BlockHash" $ \t ->
    case blockHashFromText t of
      Just h -> pure h
      Nothing -> fail $ Text.unpack $ "Invalid block hash: " <> t