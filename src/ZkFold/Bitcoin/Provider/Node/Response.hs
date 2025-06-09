module ZkFold.Bitcoin.Provider.Node.Response (
  NodeResponse (..),
) where

import Data.Aeson (FromJSON (..), Value (..), withObject, (.:))

newtype NodeResponse a = NodeResponse
  { response :: Either Value a
  }
  deriving stock (Show)

instance (FromJSON a) => FromJSON (NodeResponse a) where
  parseJSON = withObject "NodeResponse" $ \o -> do
    result <- o .: "result"
    case result of
      Null -> NodeResponse . Left <$> o .: "error"
      _ -> NodeResponse . Right <$> parseJSON result