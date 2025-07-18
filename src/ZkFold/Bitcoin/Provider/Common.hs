module ZkFold.Bitcoin.Provider.Common (
  newServantClientEnv,
  newServantClientEnvWithBasicAuth,
) where

import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTLS
import Servant.Client (
  ClientEnv,
  Scheme (..),
 )
import Servant.Client qualified as Servant

-- | Creates a new Servant 'Servant.ClientEnv' from a base url.
newServantClientEnv :: String -> IO Servant.ClientEnv
newServantClientEnv baseUrl = do
  url <- Servant.parseBaseUrl baseUrl
  manager <-
    if Servant.baseUrlScheme url == Servant.Https
      then HttpClient.newManager HttpClientTLS.tlsManagerSettings
      else HttpClient.newManager HttpClient.defaultManagerSettings
  pure $ Servant.mkClientEnv manager url

-- | Creates a new Servant 'Servant.ClientEnv' from a base url with basic authentication.
newServantClientEnvWithBasicAuth ::
  -- | The username to use for authentication.
  String ->
  -- | The password to use for authentication.
  String ->
  -- | The base URL to use for the API, like "http://localhost:8332".
  String ->
  IO ClientEnv
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