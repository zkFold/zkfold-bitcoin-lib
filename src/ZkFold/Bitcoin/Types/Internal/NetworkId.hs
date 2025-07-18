module ZkFold.Bitcoin.Types.Internal.NetworkId (
  NetworkId (..),
) where

data NetworkId
  = Mainnet
  | Testnet3
  | Testnet4
  deriving stock (Eq, Show)