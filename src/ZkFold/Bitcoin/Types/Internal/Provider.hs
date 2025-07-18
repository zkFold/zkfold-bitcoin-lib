module ZkFold.Bitcoin.Types.Internal.Provider (
  BitcoinProvider (..),
  BitcoinProviderConfig (..),
  BitcoinProviderConfigNode (..),
  providerFromConfig,
) where

import Data.Char (toLower)
import Deriving.Aeson
import ZkFold.Bitcoin.Provider.Node
import ZkFold.Bitcoin.Types.Internal.BlockHash (BlockHash)
import ZkFold.Bitcoin.Types.Internal.BlockHeader (BlockHeader)
import ZkFold.Bitcoin.Types.Internal.BlockHeight (BlockHeight)

data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier "" = ""
  getStringModifier (c : cs) = toLower c : cs

-- | Bitcoin provider configuration for connecting to a node.
data BitcoinProviderConfigNode = BitcoinProviderConfigNode
  { bpcnUsername :: !String
  , bpcnPassword :: !String
  , bpcnUrl :: !String
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "bpcn", LowerFirst]] BitcoinProviderConfigNode

-- | Bitcoin provider configuration.
newtype BitcoinProviderConfig
  = BPCNode BitcoinProviderConfigNode
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
