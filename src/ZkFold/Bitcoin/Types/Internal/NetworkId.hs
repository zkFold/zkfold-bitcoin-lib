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
  | -- \| Testnet4  -- Commented until Haskoin adds support for it.
    Regtest
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] NetworkId

networkFromId :: NetworkId -> Network
networkFromId Mainnet = btc
networkFromId Testnet3 = btcTest
networkFromId Regtest = btcRegTest
