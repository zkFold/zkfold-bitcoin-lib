module ZkFold.Bitcoin.Types.Internal.NetworkId (
  NetworkId (..),
  networkFromId,
) where

import Deriving.Aeson
import Haskoin (Network, btc, btcRegTest, btcTest)
import ZkFold.Bitcoin.Types.Internal.Common (LowerFirst)

data NetworkId
  = Mainnet
  | Testnet3
  | Testnet4
  | RegTest
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] NetworkId

-- | __Note__: Haskoin currently does not support Testnet4. We use Testnet3 details for Testnet4.
networkFromId :: NetworkId -> Network
networkFromId Mainnet = btc
networkFromId Testnet3 = btcTest
networkFromId Testnet4 = btcTest  -- TODO: Haskoin needs to add support for Testnet4.
networkFromId RegTest = btcRegTest
