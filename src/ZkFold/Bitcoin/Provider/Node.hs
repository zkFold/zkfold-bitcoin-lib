module ZkFold.Bitcoin.Provider.Node (
  nodeBlockCount,
  nodeBestBlockHash,
  nodeBlockHeader,
  nodeBlockHash,
  nodeSubmitTx,
  nodeUtxosAtAddress,
  nodeRecommendedFeeRate,
  module ZkFold.Bitcoin.Provider.Node.ApiEnv,
  NodeProviderException (..),
) where

import Control.Monad ((<=<))
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as BS16
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (serialize))
import Data.Function ((&))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Deriving.Aeson
import GHC.IsList (IsList (..))
import GHC.Natural (Natural)
import Haskoin (Address, OutPoint (..), Tx, TxHash)
import Servant.API (
  JSON,
  Post,
  ReqBody,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  ClientM,
  client,
 )
import ZkFold.Bitcoin.Provider.Node.ApiEnv
import ZkFold.Bitcoin.Provider.Node.Class (ToJSONRPC (..))
import ZkFold.Bitcoin.Provider.Node.Exception (NodeProviderException (..))
import ZkFold.Bitcoin.Provider.Node.Request (NodeRequest (..))
import ZkFold.Bitcoin.Provider.Node.Response (NodeResponse (..))
import ZkFold.Bitcoin.Types.Internal.BlockHash (BlockHash)
import ZkFold.Bitcoin.Types.Internal.BlockHeader (BlockHeader)
import ZkFold.Bitcoin.Types.Internal.BlockHeight (BlockHeight)
import ZkFold.Bitcoin.Types.Internal.Common (Bitcoin, LowerFirst, OutputIx, Satoshi, btcToSatoshi)
import ZkFold.Bitcoin.Types.Internal.UTxO

data GetBlockCount = GetBlockCount

instance ToJSONRPC GetBlockCount where
  toMethod = const "getblockcount"
  toParams = const Nothing

data GetBestBlockHash = GetBestBlockHash

instance ToJSONRPC GetBestBlockHash where
  toMethod = const "getbestblockhash"
  toParams = const Nothing

newtype GetBlockHeader = GetBlockHeader BlockHash

instance ToJSONRPC GetBlockHeader where
  toMethod = const "getblockheader"
  toParams (GetBlockHeader givenBlockHash) = Just (Aeson.Array $ fromList [toJSON givenBlockHash, Aeson.Bool False])

newtype GetBlockHash = GetBlockHash BlockHeight

instance ToJSONRPC GetBlockHash where
  toMethod = const "getblockhash"
  toParams (GetBlockHash height) = Just (Aeson.Array $ fromList [toJSON height])

newtype SubmitTx = SubmitTx Tx
instance ToJSONRPC SubmitTx where
  toMethod = const "sendrawtransaction"
  toParams (SubmitTx tx) = Just (Aeson.Array $ fromList [serialize tx & runPutS & BS16.encode & decodeUtf8 & Aeson.String])

newtype ScanTxOutSet = ScanTxOutSet Text

instance ToJSONRPC ScanTxOutSet where
  toMethod = const "scantxoutset"
  toParams (ScanTxOutSet addr) =
    Just
      ( Aeson.Array $
          fromList
            [ Aeson.String "start"
            , Aeson.Array $ fromList [Aeson.String $ "addr(" <> addr <> ")"]
            ]
      )

newtype ScanTxOutSetResponse = ScanTxOutSetResponse
  { srUnspents :: [NodeUtxo]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "sr", LowerFirst]] ScanTxOutSetResponse

data NodeUtxo = NodeUtxo
  { nuTxid :: TxHash
  , nuVout :: OutputIx
  , nuAmount :: Bitcoin
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "nu", LowerFirst]] NodeUtxo

utxoFromNodeUtxo :: Address -> NodeUtxo -> UTxO
utxoFromNodeUtxo addr (NodeUtxo txid vout amount) = UTxO (OutPoint txid vout) (btcToSatoshi amount) addr

newtype EstimateSmartFee = EstimateSmartFee Natural

instance ToJSONRPC EstimateSmartFee where
  toMethod = const "estimatesmartfee"
  toParams (EstimateSmartFee blocks) = Just (Aeson.Array $ fromList [toJSON blocks])

newtype EstimateSmartFeeResponse = EstimateSmartFeeResponse
  { esfrFeerate :: Maybe Bitcoin
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "esfr", LowerFirst]] EstimateSmartFeeResponse

type NodeApi =
  ReqBody '[JSON] (NodeRequest GetBlockCount) :> Post '[JSON] (NodeResponse BlockHeight)
    :<|> ReqBody '[JSON] (NodeRequest GetBestBlockHash) :> Post '[JSON] (NodeResponse BlockHash)
    :<|> ReqBody '[JSON] (NodeRequest GetBlockHeader) :> Post '[JSON] (NodeResponse BlockHeader)
    :<|> ReqBody '[JSON] (NodeRequest GetBlockHash) :> Post '[JSON] (NodeResponse BlockHash)
    :<|> ReqBody '[JSON] (NodeRequest SubmitTx) :> Post '[JSON] (NodeResponse TxHash)
    :<|> ReqBody '[JSON] (NodeRequest ScanTxOutSet) :> Post '[JSON] (NodeResponse ScanTxOutSetResponse)
    :<|> ReqBody '[JSON] (NodeRequest EstimateSmartFee) :> Post '[JSON] (NodeResponse EstimateSmartFeeResponse)

blockCount :: NodeRequest GetBlockCount -> ClientM (NodeResponse BlockHeight)
bestBlockHash :: NodeRequest GetBestBlockHash -> ClientM (NodeResponse BlockHash)
blockHeader :: NodeRequest GetBlockHeader -> ClientM (NodeResponse BlockHeader)
blockHash :: NodeRequest GetBlockHash -> ClientM (NodeResponse BlockHash)
submitTx :: NodeRequest SubmitTx -> ClientM (NodeResponse TxHash)
scanTxOutSet :: NodeRequest ScanTxOutSet -> ClientM (NodeResponse ScanTxOutSetResponse)
estimateSmartFee :: NodeRequest EstimateSmartFee -> ClientM (NodeResponse EstimateSmartFeeResponse)
blockCount
  :<|> bestBlockHash
  :<|> blockHeader
  :<|> blockHash
  :<|> submitTx
  :<|> scanTxOutSet
  :<|> estimateSmartFee = client @NodeApi Proxy

nodeBlockCount :: NodeApiEnv -> IO BlockHeight
nodeBlockCount env =
  handleNodeError "nodeBlockCount" <=< runNodeClient env $ blockCount (NodeRequest GetBlockCount)

nodeBestBlockHash :: NodeApiEnv -> IO BlockHash
nodeBestBlockHash env =
  handleNodeError "nodeBestBlockHash" <=< runNodeClient env $ bestBlockHash (NodeRequest GetBestBlockHash)

nodeBlockHeader :: NodeApiEnv -> BlockHash -> IO BlockHeader
nodeBlockHeader env givenBlockHash =
  handleNodeError "nodeBlockHeader" <=< runNodeClient env $ blockHeader (NodeRequest (GetBlockHeader givenBlockHash))

nodeBlockHash :: NodeApiEnv -> BlockHeight -> IO BlockHash
nodeBlockHash env givenHeight =
  handleNodeError "nodeBlockHash" <=< runNodeClient env $ blockHash (NodeRequest (GetBlockHash givenHeight))

nodeSubmitTx :: NodeApiEnv -> Tx -> IO TxHash
nodeSubmitTx env tx =
  handleNodeError "nodeSubmitTx" <=< runNodeClient env $ submitTx (NodeRequest (SubmitTx tx))

-- TODO: To include for mempool outputs?
nodeUtxosAtAddress :: NodeApiEnv -> (Text, Address) -> IO [UTxO]
nodeUtxosAtAddress env (addrText, addr) = do
  ScanTxOutSetResponse{..} <- handleNodeError "nodeUtxosAtAddress" <=< runNodeClient env $ scanTxOutSet (NodeRequest (ScanTxOutSet addrText))
  pure $ fmap (utxoFromNodeUtxo addr) srUnspents

-- TODO: Need to also test this against mainnet.
nodeRecommendedFeeRate :: NodeApiEnv -> IO Satoshi
nodeRecommendedFeeRate env = do
  EstimateSmartFeeResponse{..} <- handleNodeError "nodeRecommendedFeeRate" <=< runNodeClient env $ estimateSmartFee (NodeRequest (EstimateSmartFee 1))
  pure $ maybe 1 (\fr -> btcToSatoshi fr `div` 1000) esfrFeerate