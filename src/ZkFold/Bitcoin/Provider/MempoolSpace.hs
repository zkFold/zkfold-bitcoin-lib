module ZkFold.Bitcoin.Provider.MempoolSpace (
  MempoolSpaceApiEnv,
  mempoolSpaceApiEnvToClientEnv,
  newMempoolSpaceApiEnv,
  mempoolSpaceBlockCount,
  mempoolSpaceBlockTipHash,
  mempoolSpaceBlockHeader,
  mempoolSpaceBlockHash,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Data (Typeable)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (typeOf)
import Network.HTTP.Media qualified as M
import Servant.API (
  Accept (..),
  Capture,
  Get,
  MimeUnrender (..),
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  ClientEnv,
  ClientM,
  client,
 )
import Servant.Client qualified as Servant
import Text.Read (readMaybe)
import ZkFold.Bitcoin.Provider.Common (newServantClientEnv)
import ZkFold.Bitcoin.Types.Internal.BlockHash
import ZkFold.Bitcoin.Types.Internal.BlockHeader
import ZkFold.Bitcoin.Types.Internal.BlockHeight
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId (..))

newtype MempoolSpaceApiEnv = MempoolSpaceApiEnv ClientEnv

mempoolSpaceApiEnvToClientEnv :: MempoolSpaceApiEnv -> ClientEnv
mempoolSpaceApiEnvToClientEnv (MempoolSpaceApiEnv cEnv) = cEnv

-- | Create a new 'MempoolSpaceApiEnv'.
newMempoolSpaceApiEnv ::
  NetworkId ->
  IO MempoolSpaceApiEnv
newMempoolSpaceApiEnv nid = do
  cEnv <-
    newServantClientEnv
      ( case nid of
          Mainnet -> "https://mempool.space/api"
          Testnet3 -> "https://mempool.space/testnet/api"
          Testnet4 -> "https://mempool.space/testnet4/api"
      )
  return $ MempoolSpaceApiEnv cEnv

runMempoolSpaceClient :: MempoolSpaceApiEnv -> Servant.ClientM a -> IO (Either Servant.ClientError a)
runMempoolSpaceClient (MempoolSpaceApiEnv cEnv) c = Servant.runClientM c cEnv

data MempoolSpaceApiError
  = -- | Error from API.
    MempoolSpaceApiError Text Servant.ClientError
  deriving stock (Show)
  deriving anyclass (Exception)

handleMempoolSpaceError :: Text -> Either Servant.ClientError a -> IO a
handleMempoolSpaceError locationInfo =
  either
    (throwIO . MempoolSpaceApiError locationInfo)
    pure

-- @PlainText@ expects content type to also have a charset but mempool.space returns without it.
data TextPlain

instance Accept TextPlain where
  contentType _ = ("text" :: ByteString) M.// ("plain" :: ByteString)

newtype PlainTextRead a = PlainTextRead {unPlainTextRead :: a}

instance (Read a, Typeable a) => MimeUnrender TextPlain (PlainTextRead a) where
  mimeUnrender _ bs = case readMaybe (BS8.unpack (BS8.toStrict bs)) of
    Just x -> Right (PlainTextRead x)
    Nothing -> Left $ "Invalid format for " <> show (typeOf (Proxy :: Proxy a))

type MempoolSpaceApi =
  "blocks" :> "tip" :> "height" :> Get '[TextPlain] (PlainTextRead BlockHeight) -- Unfortunately, mempool.space returns a plain text response instead of JSON.
    :<|> "blocks" :> "tip" :> "hash" :> Get '[TextPlain] (PlainTextRead BlockHash)
    :<|> "block" :> Capture "blockHash" BlockHash :> "header" :> Get '[TextPlain] (PlainTextRead BlockHeader)
    :<|> "block-height" :> Capture "blockHeight" BlockHeight :> Get '[TextPlain] (PlainTextRead BlockHash)

blockCount :: ClientM (PlainTextRead BlockHeight)
blockTipHash :: ClientM (PlainTextRead BlockHash)
blockHeader :: BlockHash -> ClientM (PlainTextRead BlockHeader)
blockHash :: BlockHeight -> ClientM (PlainTextRead BlockHash)
blockCount :<|> blockTipHash :<|> blockHeader :<|> blockHash = client @MempoolSpaceApi Proxy

mempoolSpaceBlockCount :: MempoolSpaceApiEnv -> IO BlockHeight
mempoolSpaceBlockCount env =
  handleMempoolSpaceError "mempoolSpaceBlockCount" . fmap unPlainTextRead <=< runMempoolSpaceClient env $ blockCount

mempoolSpaceBlockTipHash :: MempoolSpaceApiEnv -> IO BlockHash
mempoolSpaceBlockTipHash env =
  handleMempoolSpaceError "mempoolSpaceBlockTipHash" . fmap unPlainTextRead <=< runMempoolSpaceClient env $ blockTipHash

mempoolSpaceBlockHeader :: MempoolSpaceApiEnv -> BlockHash -> IO BlockHeader
mempoolSpaceBlockHeader env bh =
  handleMempoolSpaceError "mempoolSpaceBlockHeader" . fmap unPlainTextRead <=< runMempoolSpaceClient env $ blockHeader bh

mempoolSpaceBlockHash :: MempoolSpaceApiEnv -> BlockHeight -> IO BlockHash
mempoolSpaceBlockHash env bh =
  handleMempoolSpaceError "mempoolSpaceBlockHash" . fmap unPlainTextRead <=< runMempoolSpaceClient env $ blockHash bh