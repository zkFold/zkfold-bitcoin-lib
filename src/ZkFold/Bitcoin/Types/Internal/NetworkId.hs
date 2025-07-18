module ZkFold.Bitcoin.Types.Internal.NetworkId (
  NetworkId (..),
) where

import Deriving.Aeson
import ZkFold.Bitcoin.Types.Internal.Common (LowerFirst)

data NetworkId
  = Mainnet
  | Testnet3
  -- \| Testnet4  -- Commented until Haskoin adds support for it.
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] NetworkId