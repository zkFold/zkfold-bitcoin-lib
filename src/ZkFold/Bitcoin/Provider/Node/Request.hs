module ZkFold.Bitcoin.Provider.Node.Request (NodeRequest (..)) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..))

newtype NodeRequest a = NodeRequest a

instance (ToJSONRPC a) => ToJSON (NodeRequest a) where
  toJSON (NodeRequest a) =
    object
      [ "jsonrpc" .= ("1.0" :: Text) -- TODO: Should it be 2.0?
      , "method" .= toMethod a
      , "params" .= toParams a
      ]