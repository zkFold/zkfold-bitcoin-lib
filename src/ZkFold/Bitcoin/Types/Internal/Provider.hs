module ZkFold.Bitcoin.Types.Internal.Provider (
  BitcoinProvider (..),
  BitcoinProviderConfig (..),
  BitcoinProviderConfigNode (..),
  providerFromConfig,
) where

import Deriving.Aeson
import ZkFold.Bitcoin.Provider.MempoolSpace
import ZkFold.Bitcoin.Provider.Node
import ZkFold.Bitcoin.Types.Internal.BlockHash (BlockHash)
import ZkFold.Bitcoin.Types.Internal.BlockHeader (BlockHeader)
import ZkFold.Bitcoin.Types.Internal.BlockHeight (BlockHeight)
import ZkFold.Bitcoin.Types.Internal.Common (LowerFirst)
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId)

-- | Bitcoin provider configuration for connecting to a node.
data BitcoinProviderConfigNode = BitcoinProviderConfigNode
  { bpcnUsername :: !String
  , bpcnPassword :: !String
  , bpcnUrl :: !String
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "bpcn", LowerFirst]] BitcoinProviderConfigNode

-- | Bitcoin provider configuration.
data BitcoinProviderConfig
  = BPCNode BitcoinProviderConfigNode
  | BPCMempoolSpace NetworkId
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "BPC", LowerFirst]] BitcoinProviderConfig

-- | Bitcoin provider.
data BitcoinProvider = BitcoinProvider
  { bpBlockCount :: IO BlockHeight
  , bpBestBlockHash :: IO BlockHash
  , bpBlockHeader :: BlockHash -> IO BlockHeader
  , bpBlockHash :: BlockHeight -> IO BlockHash
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
      }
providerFromConfig (BPCMempoolSpace nid) = do
  env <- newMempoolSpaceApiEnv nid
  pure $
    BitcoinProvider
      { bpBlockCount = mempoolSpaceBlockCount env
      , bpBestBlockHash = mempoolSpaceBlockTipHash env
      , bpBlockHeader = mempoolSpaceBlockHeader env
      , bpBlockHash = mempoolSpaceBlockHash env
      }