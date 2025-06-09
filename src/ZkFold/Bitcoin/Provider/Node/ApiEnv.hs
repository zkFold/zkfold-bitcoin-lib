module ZkFold.Bitcoin.Provider.Node.ApiEnv (
  NodeApiEnv,
  nodeToClientEnv,
  newNodeApiEnv,
  runNodeClient,
  handleNodeError,
) where

import Control.Exception (throwIO)
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTLS
import Servant.Client (
  ClientEnv,
  Scheme (..),
 )
import Servant.Client qualified as Servant
import ZkFold.Bitcoin.Provider.Node.Exception (NodeProviderException (..))
import ZkFold.Bitcoin.Provider.Node.Response (NodeResponse (..))

newServantClientEnvWithBasicAuth :: String -> String -> String -> IO ClientEnv
newServantClientEnvWithBasicAuth username password baseUrl = do
  url <- Servant.parseBaseUrl baseUrl
  manager <-
    if Servant.baseUrlScheme url == Https
      then
        HttpClient.newManager $
          mkAuthManager HttpClientTLS.tlsManagerSettings
      else
        HttpClient.newManager $ mkAuthManager HttpClient.defaultManagerSettings
  return $ Servant.mkClientEnv manager url
 where
  mkAuthManager manager =
    manager
      { HttpClient.managerModifyRequest = return . HttpClient.applyBasicAuth (BS8.pack username) (BS8.pack password)
      }

newtype NodeApiEnv = NodeApiEnv ClientEnv

nodeToClientEnv :: NodeApiEnv -> ClientEnv
nodeToClientEnv (NodeApiEnv cEnv) = cEnv

-- | Create a new 'NodeApiEnv' with the given username, password, and base URL.
newNodeApiEnv ::
  -- | The username to use for authentication.
  String ->
  -- | The password to use for authentication.
  String ->
  -- | The base URL to use for the API, like "http://localhost:8332".
  String ->
  IO NodeApiEnv
newNodeApiEnv username password baseUrl = do
  cEnv <- newServantClientEnvWithBasicAuth username password baseUrl
  return $ NodeApiEnv cEnv

runNodeClient :: NodeApiEnv -> Servant.ClientM a -> IO (Either Servant.ClientError a)
runNodeClient (NodeApiEnv cEnv) c = Servant.runClientM c cEnv

handleNodeError :: Text -> Either Servant.ClientError (NodeResponse a) -> IO a
handleNodeError locationInfo =
  either
    (throwIO . NodeApiError locationInfo)
    -- `NodeResponse` would likely be `Right` as in case of error, we are in `ClientError` case. We need to make use of something like `WithStatus` for `NodeErrorResponse` to be actually useful.
    (`reduceNodeResponse` (throwIO . NodeErrorResponse locationInfo))

{-# INLINEABLE reduceNodeResponse #-}
reduceNodeResponse :: (Applicative f) => NodeResponse a -> (Value -> f a) -> f a
reduceNodeResponse res e = case response res of
  Left err -> e err
  Right a -> pure a