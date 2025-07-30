{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Bitcoin.Provider.MempoolSpace (
  MempoolSpaceApiEnv,
  mempoolSpaceApiEnvToClientEnv,
  newMempoolSpaceApiEnv,
  mempoolSpaceBlockCount,
  mempoolSpaceBlockTipHash,
  mempoolSpaceBlockHeader,
  mempoolSpaceBlockHash,
  mempoolSpaceUtxosAtAddress,
  mempoolSpaceSubmitTx,
  mempoolSpaceRecommendedFeeRate,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Data (Typeable)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (typeOf)
import Deriving.Aeson
import Haskoin (Address, OutPoint (..), Tx, TxHash)
import Network.HTTP.Media qualified as M
import Servant.API (
  Accept (..),
  Capture,
  Get,
  JSON,
  MimeRender (..),
  MimeUnrender (..),
  PlainText,
  Post,
  ReqBody,
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
import ZkFold.Bitcoin.Types.Internal.Common (LowerFirst, OutputIx, Satoshi)
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId (..))
import ZkFold.Bitcoin.Types.Internal.UTxO

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
          RegTest -> error "RegTest is not supported by mempool.space"
          -- Testnet4 -> "https://mempool.space/testnet4/api"
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

instance MimeRender PlainText Tx where
  mimeRender _ = BS8.fromStrict . BS16.encode . runPutS . serialize

data MempoolSpaceUtxo = MempoolSpaceUtxo
  { msuTxid :: TxHash
  , msuVout :: OutputIx
  , msuValue :: Satoshi
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "msu", LowerFirst]] MempoolSpaceUtxo

utxoFromMempoolSpaceUtxo :: Address -> MempoolSpaceUtxo -> UTxO
utxoFromMempoolSpaceUtxo addr (MempoolSpaceUtxo txid vout value) = UTxO (OutPoint txid vout) value addr

newtype MempoolSpaceFeeResponse = MempoolSpaceFeeResponse
  { msfFastestFee :: Satoshi
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "msf", LowerFirst]] MempoolSpaceFeeResponse

type MempoolSpaceApi =
  "blocks" :> "tip" :> "height" :> Get '[TextPlain] (PlainTextRead BlockHeight) -- Unfortunately, mempool.space returns a plain text response instead of JSON.
    :<|> "blocks" :> "tip" :> "hash" :> Get '[TextPlain] (PlainTextRead BlockHash)
    :<|> "block" :> Capture "blockHash" BlockHash :> "header" :> Get '[TextPlain] (PlainTextRead BlockHeader)
    :<|> "block-height" :> Capture "blockHeight" BlockHeight :> Get '[TextPlain] (PlainTextRead BlockHash)
    :<|> "address" :> Capture "address" Text :> "utxo" :> Get '[JSON] [MempoolSpaceUtxo]
    :<|> "tx" :> ReqBody '[PlainText] Tx :> Post '[TextPlain] (PlainTextRead TxHash)
    :<|> "v1" :> "fees" :> "recommended" :> Get '[JSON] MempoolSpaceFeeResponse

blockCount :: ClientM (PlainTextRead BlockHeight)
blockTipHash :: ClientM (PlainTextRead BlockHash)
blockHeader :: BlockHash -> ClientM (PlainTextRead BlockHeader)
blockHash :: BlockHeight -> ClientM (PlainTextRead BlockHash)
addressUtxos :: Text -> ClientM [MempoolSpaceUtxo]
txHash :: Tx -> ClientM (PlainTextRead TxHash)
recommendedFeeRate :: ClientM MempoolSpaceFeeResponse
blockCount :<|> blockTipHash :<|> blockHeader :<|> blockHash :<|> addressUtxos :<|> txHash :<|> recommendedFeeRate = client @MempoolSpaceApi Proxy

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

-- TODO: Does it return for mempool outputs?
mempoolSpaceUtxosAtAddress :: MempoolSpaceApiEnv -> (Text, Address) -> IO [UTxO]
mempoolSpaceUtxosAtAddress env (addrText, addr) =
  handleMempoolSpaceError "mempoolSpaceUtxosAtAddress" . fmap (fmap (utxoFromMempoolSpaceUtxo addr)) <=< runMempoolSpaceClient env $ addressUtxos addrText

-- TODO: Need to test this!
mempoolSpaceSubmitTx :: MempoolSpaceApiEnv -> Tx -> IO TxHash
mempoolSpaceSubmitTx env tx =
  handleMempoolSpaceError "mempoolSpaceSubmitTx" . fmap unPlainTextRead <=< runMempoolSpaceClient env $ txHash tx

mempoolSpaceRecommendedFeeRate :: MempoolSpaceApiEnv -> IO Satoshi
mempoolSpaceRecommendedFeeRate env =
  handleMempoolSpaceError "mempoolSpaceRecommendedFeeRate" . fmap (\MempoolSpaceFeeResponse{..} -> msfFastestFee) <=< runMempoolSpaceClient env $ recommendedFeeRate