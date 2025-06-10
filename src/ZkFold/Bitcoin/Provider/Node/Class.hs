module ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..)) where

import Data.Aeson (Value)
import Data.Text (Text)

-- | Class for types that can be converted to JSON-RPC requests.
class ToJSONRPC a where
  toMethod :: a -> Text
  toParams :: a -> Maybe Value