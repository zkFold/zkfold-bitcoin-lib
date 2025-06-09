module ZkFold.Bitcoin.Provider.Node () where

import Control.Monad ((<=<))
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as BS8
import Data.Proxy (Proxy (..))
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
import ZkFold.Bitcoin.Provider.Node.ApiEnv (NodeApiEnv, handleNodeError, runNodeClient)
import ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..))
import ZkFold.Bitcoin.Provider.Node.Request (NodeRequest (..))
import ZkFold.Bitcoin.Provider.Node.Response (NodeResponse (..))

data GetBestBlockHash = GetBestBlockHash

instance ToJSONRPC GetBestBlockHash where
  toMethod = const "getbestblockhash"
  toParams = const Nothing

type NodeApi =
  ReqBody '[JSON] (NodeRequest GetBestBlockHash) :> Post '[JSON] (NodeResponse Text)

bestBlockHash :: NodeRequest GetBestBlockHash -> ClientM (NodeResponse Text)
bestBlockHash = client @NodeApi Proxy

nodeBestBlockHash :: NodeApiEnv -> IO Text
nodeBestBlockHash env =
  handleNodeError "nodeBestBlockHash" <=< runNodeClient env $ bestBlockHash (NodeRequest GetBestBlockHash)