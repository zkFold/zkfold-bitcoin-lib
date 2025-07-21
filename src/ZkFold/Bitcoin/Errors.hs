module ZkFold.Bitcoin.Errors (
  BitcoinQueryMonadException (..),
) where

import Control.Exception (Exception)
import Haskoin (Address)
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId)

data BitcoinQueryMonadException
  = UnableToSerializeAddress Address NetworkId
  deriving stock (Show)
  deriving anyclass (Exception)