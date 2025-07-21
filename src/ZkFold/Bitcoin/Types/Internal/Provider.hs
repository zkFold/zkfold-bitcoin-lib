module ZkFold.Bitcoin.Types.Internal.Provider (
  BitcoinProvider (..),
  BitcoinProviderConfig (..),
  BitcoinProviderConfigNode (..),
  providerFromConfig,
) where

import Control.Exception (throwIO)
import Data.Text (Text)
import Deriving.Aeson
import Haskoin (Address, Tx, TxHash, addrToText)
import ZkFold.Bitcoin.Errors (BitcoinQueryMonadException (..))
import ZkFold.Bitcoin.Provider.MempoolSpace
import ZkFold.Bitcoin.Provider.Node
import ZkFold.Bitcoin.Types.Internal.BlockHash (BlockHash)
import ZkFold.Bitcoin.Types.Internal.BlockHeader (BlockHeader)
import ZkFold.Bitcoin.Types.Internal.BlockHeight (BlockHeight)
import ZkFold.Bitcoin.Types.Internal.Common (LowerFirst)
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId, networkFromId)
import ZkFold.Bitcoin.Types.Internal.UTxO (UTxO)

-- | Bitcoin provider configuration for connecting to a node.
data BitcoinProviderConfigNode = BitcoinProviderConfigNode
  { bpcnUsername :: !String
  , bpcnPassword :: !String
  , bpcnUrl :: !String
  , bpcnNetworkId :: !NetworkId
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "bpcn", LowerFirst]] BitcoinProviderConfigNode

newtype BitcoinProviderConfigMempoolSpace = BitcoinProviderConfigMempoolSpace
  { bpcmsNetworkId :: NetworkId
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "bpcms", LowerFirst]] BitcoinProviderConfigMempoolSpace

-- | Bitcoin provider configuration.
data BitcoinProviderConfig
  = BPCNode BitcoinProviderConfigNode
  | BPCMempoolSpace BitcoinProviderConfigMempoolSpace
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "BPC", LowerFirst]] BitcoinProviderConfig

-- | Bitcoin provider.
data BitcoinProvider = BitcoinProvider
  { bpBlockCount :: IO BlockHeight
  , bpBestBlockHash :: IO BlockHash
  , bpBlockHeader :: BlockHash -> IO BlockHeader
  , bpBlockHash :: BlockHeight -> IO BlockHash
  , bpUtxosAtAddress :: Address -> IO [UTxO]
  , bpSubmitTx :: Tx -> IO TxHash
  , bpNetworkId :: NetworkId
  }

-- | Create a 'BitcoinProvider' from a 'BitcoinProviderConfig'.
providerFromConfig :: BitcoinProviderConfig -> IO BitcoinProvider
providerFromConfig (BPCNode (BitcoinProviderConfigNode{..})) = do
  env <- newNodeApiEnv bpcnUsername bpcnPassword bpcnUrl
  pure $
    BitcoinProvider
      { bpBlockCount = nodeBlockCount env
      , bpBestBlockHash = nodeBestBlockHash env
      , bpBlockHeader = nodeBlockHeader env
      , bpBlockHash = nodeBlockHash env
      , bpUtxosAtAddress = \addr -> do
          addrText <- resolveAddress addr bpcnNetworkId
          nodeUtxosAtAddress env addrText
      , bpSubmitTx = nodeSubmitTx env
      , bpNetworkId = bpcnNetworkId
      }
providerFromConfig (BPCMempoolSpace (BitcoinProviderConfigMempoolSpace{..})) = do
  env <- newMempoolSpaceApiEnv bpcmsNetworkId
  pure $
    BitcoinProvider
      { bpBlockCount = mempoolSpaceBlockCount env
      , bpBestBlockHash = mempoolSpaceBlockTipHash env
      , bpBlockHeader = mempoolSpaceBlockHeader env
      , bpBlockHash = mempoolSpaceBlockHash env
      , bpUtxosAtAddress = \addr -> do
          addrText <- resolveAddress addr bpcmsNetworkId
          mempoolSpaceUtxosAtAddress env addrText
      , bpSubmitTx = mempoolSpaceSubmitTx env
      , bpNetworkId = bpcmsNetworkId
      }

resolveAddress :: Address -> NetworkId -> IO Text
resolveAddress addr nid =
  case addrToText (networkFromId nid) addr of
    Nothing -> throwIO $ UnableToSerializeAddress addr nid
    Just addrText' -> pure addrText'