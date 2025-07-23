module ZkFold.Bitcoin.Errors (
  BitcoinMonadException (..),
) where

import Control.Exception (Exception)
import Haskoin (Address)
import ZkFold.Bitcoin.Types.Internal.Common
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId)
import ZkFold.Bitcoin.Types.Internal.UTxO

data BitcoinMonadException
  = UnableToSerializeAddress Address NetworkId
  | UnableToChooseCoins [UTxO] Satoshi FeePerByte String
  deriving stock (Show)
  deriving anyclass (Exception)