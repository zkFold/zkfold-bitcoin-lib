module ZkFold.Bitcoin.Provider.Node (
  nodeBestBlockHash,
  module ZkFold.Bitcoin.Provider.Node.ApiEnv,
) where

import Control.Monad ((<=<))
import Data.Aeson (ToJSON (..), Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.IsList (IsList (..))
import GHC.Natural (Natural)
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
import ZkFold.Bitcoin.Provider.Node.ApiEnv
import ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..))
import ZkFold.Bitcoin.Provider.Node.Request (NodeRequest (..))
import ZkFold.Bitcoin.Provider.Node.Response (NodeResponse (..))

data GetBestBlockHash = GetBestBlockHash

instance ToJSONRPC GetBestBlockHash where
  toMethod = const "getbestblockhash"
  toParams = const Nothing

data GetBlockHeader = GetBlockHeader Text

instance ToJSONRPC GetBlockHeader where
  toMethod = const "getblockheader"
  toParams (GetBlockHeader blockHash) = Just (Aeson.Array $ fromList [toJSON blockHash, Aeson.Bool False])

data GetBlockHash = GetBlockHash Natural

instance ToJSONRPC GetBlockHash where
  toMethod = const "getblockhash"
  toParams (GetBlockHash height) = Just (Aeson.Array $ fromList [toJSON height])

type NodeApi =
  ReqBody '[JSON] (NodeRequest GetBestBlockHash) :> Post '[JSON] (NodeResponse Text)
    :<|> ReqBody '[JSON] (NodeRequest GetBlockHeader) :> Post '[JSON] (NodeResponse Text)
    :<|> ReqBody '[JSON] (NodeRequest GetBlockHash) :> Post '[JSON] (NodeResponse Text)

-- bestBlockHash :: NodeRequest GetBestBlockHash -> ClientM (NodeResponse Text)
bestBlockHash :<|> blockHeader :<|> blockHash = client @NodeApi Proxy

nodeBestBlockHash :: NodeApiEnv -> IO Text
nodeBestBlockHash env =
  handleNodeError "nodeBestBlockHash" <=< runNodeClient env $ bestBlockHash (NodeRequest GetBestBlockHash)

nodeBlockHeader :: NodeApiEnv -> Text -> IO Text
nodeBlockHeader env givenBlockHash =
  handleNodeError "nodeBlockHeader" <=< runNodeClient env $ blockHeader (NodeRequest (GetBlockHeader givenBlockHash))

nodeBlockHash :: NodeApiEnv -> Natural -> IO Text
nodeBlockHash env givenHeight =
  handleNodeError "nodeBlockHash" <=< runNodeClient env $ blockHash (NodeRequest (GetBlockHash givenHeight))