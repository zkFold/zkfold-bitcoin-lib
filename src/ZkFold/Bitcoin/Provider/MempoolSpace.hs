{-# LANGUAGE GADTs #-}
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
  mempoolSpaceTxConfirmations,
  mempoolSpaceRecommendedFeeRate,
  mempoolSpaceWaitForTxConfirmations,
  MempoolSpaceApiError (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Data (Typeable, type (:~~:) (HRefl))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Deriving.Aeson
import Haskoin (Address, OutPoint (..), Tx, TxHash, hexToTxHash)
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
import Type.Reflection (eqTypeRep, typeRep)
import ZkFold.Bitcoin.Errors (BitcoinMonadException (..))
import ZkFold.Bitcoin.Provider.Common (newServantClientEnv)
import ZkFold.Bitcoin.Types.Internal.BlockHash
import ZkFold.Bitcoin.Types.Internal.BlockHeader
import ZkFold.Bitcoin.Types.Internal.BlockHeight
import ZkFold.Bitcoin.Types.Internal.Common (LowerFirst, OutputIx, Satoshi)
import ZkFold.Bitcoin.Types.Internal.Confirmations (TxConfirmationsConfig (..), pollIntervalMicros)
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
  mimeUnrender _ bs =
    let responseString = BS8.unpack (BS8.toStrict bs)
     in case eqTypeRep (typeRep @a) (typeRep @TxHash) of
          Just HRefl ->
            case hexToTxHash (Text.pack responseString) of
              Just txh -> Right (PlainTextRead txh)
              Nothing -> Left $ "Invalid hex string for " <> show (typeRep @TxHash) <> " in response: " <> responseString
          Nothing ->
            case readMaybe responseString of
              Just x -> Right (PlainTextRead x)
              Nothing -> Left $ "Invalid format for " <> show (typeRep @a) <> " in response: " <> responseString

instance MimeRender PlainText Tx where
  mimeRender _ = BS8.fromStrict . BS16.encode . runPutS . serialize

data MempoolSpaceUtxoStatus = MempoolSpaceUtxoStatus
  { msusConfirmed :: Bool
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "msus", LowerFirst]] MempoolSpaceUtxoStatus

data MempoolSpaceUtxo = MempoolSpaceUtxo
  { msuTxid :: TxHash
  , msuVout :: OutputIx
  , msuValue :: Satoshi
  , msuStatus :: MempoolSpaceUtxoStatus
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "msu", LowerFirst]] MempoolSpaceUtxo

utxoFromMempoolSpaceUtxo :: Address -> MempoolSpaceUtxo -> UTxO
utxoFromMempoolSpaceUtxo addr (MempoolSpaceUtxo txid vout value _) = UTxO (OutPoint txid vout) value addr

newtype MempoolSpaceFeeResponse = MempoolSpaceFeeResponse
  { msfFastestFee :: Satoshi
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "msf", LowerFirst]] MempoolSpaceFeeResponse

data MempoolSpaceTxStatus = MempoolSpaceTxStatus
  { mstsConfirmed :: Bool
  , mstsBlockHeight :: Maybe BlockHeight
  }
  deriving stock (Show, Generic)

instance FromJSON MempoolSpaceTxStatus where
  parseJSON =
    Aeson.withObject "MempoolSpaceTxStatus" $ \obj ->
      MempoolSpaceTxStatus
        <$> obj .: "confirmed"
        <*> obj .:? "block_height"

type MempoolSpaceApi =
  "blocks" :> "tip" :> "height" :> Get '[TextPlain] (PlainTextRead BlockHeight) -- Unfortunately, mempool.space returns a plain text response instead of JSON.
    :<|> "blocks" :> "tip" :> "hash" :> Get '[TextPlain] (PlainTextRead BlockHash)
    :<|> "block" :> Capture "blockHash" BlockHash :> "header" :> Get '[TextPlain] (PlainTextRead BlockHeader)
    :<|> "block-height" :> Capture "blockHeight" BlockHeight :> Get '[TextPlain] (PlainTextRead BlockHash)
    :<|> "address" :> Capture "address" Text :> "utxo" :> Get '[JSON] [MempoolSpaceUtxo]
    :<|> "tx" :> ReqBody '[PlainText] Tx :> Post '[TextPlain] (PlainTextRead TxHash)
    :<|> "tx" :> Capture "txid" Text :> "status" :> Get '[JSON] MempoolSpaceTxStatus
    :<|> "v1" :> "fees" :> "recommended" :> Get '[JSON] MempoolSpaceFeeResponse

blockCount :: ClientM (PlainTextRead BlockHeight)
blockTipHash :: ClientM (PlainTextRead BlockHash)
blockHeader :: BlockHash -> ClientM (PlainTextRead BlockHeader)
blockHash :: BlockHeight -> ClientM (PlainTextRead BlockHash)
addressUtxos :: Text -> ClientM [MempoolSpaceUtxo]
txHash :: Tx -> ClientM (PlainTextRead TxHash)
txStatus :: Text -> ClientM MempoolSpaceTxStatus
recommendedFeeRate :: ClientM MempoolSpaceFeeResponse
blockCount :<|> blockTipHash :<|> blockHeader :<|> blockHash :<|> addressUtxos :<|> txHash :<|> txStatus :<|> recommendedFeeRate = client @MempoolSpaceApi Proxy

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
  handleMempoolSpaceError "mempoolSpaceUtxosAtAddress"
    . fmap (fmap (utxoFromMempoolSpaceUtxo addr) . filter (msusConfirmed . msuStatus))
    <=< runMempoolSpaceClient env
    $ addressUtxos addrText

mempoolSpaceSubmitTx :: MempoolSpaceApiEnv -> Tx -> IO TxHash
mempoolSpaceSubmitTx env tx =
  handleMempoolSpaceError "mempoolSpaceSubmitTx" . fmap unPlainTextRead <=< runMempoolSpaceClient env $ txHash tx

mempoolSpaceRecommendedFeeRate :: MempoolSpaceApiEnv -> IO Satoshi
mempoolSpaceRecommendedFeeRate env =
  handleMempoolSpaceError "mempoolSpaceRecommendedFeeRate" . fmap (\MempoolSpaceFeeResponse{..} -> msfFastestFee) <=< runMempoolSpaceClient env $ recommendedFeeRate

mempoolSpaceTxConfirmations :: MempoolSpaceApiEnv -> TxHash -> IO BlockHeight
mempoolSpaceTxConfirmations env txId = do
  MempoolSpaceTxStatus{..} <- handleMempoolSpaceError "mempoolSpaceTxStatus" <=< runMempoolSpaceClient env $ txStatus (txHashToText txId)
  if not mstsConfirmed
    then pure 0
    else case mstsBlockHeight of
      Nothing -> pure 0
      Just confirmedHeight -> do
        tipHeight <- mempoolSpaceBlockCount env
        if tipHeight >= confirmedHeight
          then pure (tipHeight - confirmedHeight + 1)
          else pure 0

mempoolSpaceWaitForTxConfirmations :: MempoolSpaceApiEnv -> TxHash -> TxConfirmationsConfig -> IO ()
mempoolSpaceWaitForTxConfirmations env txId config
  | tccConfirmations config == 0 = pure ()
  | tccMaxAttempts config == 0 = throwIO $ WaitForTxConfirmationsTimeout txId config Nothing
  | otherwise = loop 1 Nothing
 where
  loop attempt lastKnown =
    do
      confirmations <- mempoolSpaceTxConfirmations env txId
      let lastKnown' =
            if confirmations == 0
              then lastKnown
              else Just confirmations
      if confirmations >= tccConfirmations config
        then pure ()
        else waitAndRetry attempt lastKnown'
  waitAndRetry attempt lastKnown =
    if attempt >= tccMaxAttempts config
      then
        throwIO $ WaitForTxConfirmationsTimeout txId config lastKnown
      else do
        threadDelay $ pollIntervalMicros config
        loop (attempt + 1) lastKnown

txHashToText :: TxHash -> Text
txHashToText txHashValue =
  case Aeson.toJSON txHashValue of
    Aeson.String txHashText -> txHashText
    _ -> error "TxHash JSON encoding is not a string"
