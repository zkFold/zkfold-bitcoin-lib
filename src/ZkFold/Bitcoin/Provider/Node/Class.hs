module ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..)) where

import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTLS
import Servant.API (
  JSON,
  Post,
  ReqBody,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  BaseUrl (..),
  ClientEnv,
  ClientError,
  ClientM,
  Scheme (..),
  baseUrl,
  client,
  parseBaseUrl,
  runClientM,
 )
import Servant.Client qualified as Servant

class ToJSONRPC a where
  toMethod :: a -> Text
  toParams :: a -> Maybe Value