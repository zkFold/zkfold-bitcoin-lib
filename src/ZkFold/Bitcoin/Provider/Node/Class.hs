module ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..)) where

import Data.Aeson (Value)
import Data.Text (Text)

class ToJSONRPC a where
  toMethod :: a -> Text
  toParams :: a -> Maybe Value