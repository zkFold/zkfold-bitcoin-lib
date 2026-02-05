module ZkFold.Bitcoin.Errors (
  BitcoinMonadException (..),
) where

import Control.Exception (Exception)
import Haskoin (Address, Tx, TxHash)
import ZkFold.Bitcoin.Types.Internal.BlockHeight (BlockHeight)
import ZkFold.Bitcoin.Types.Internal.Common
import ZkFold.Bitcoin.Types.Internal.Confirmations (TxConfirmationsConfig)
import ZkFold.Bitcoin.Types.Internal.NetworkId (NetworkId)
import ZkFold.Bitcoin.Types.Internal.Skeleton (TxSkeleton)
import ZkFold.Bitcoin.Types.Internal.UTxO

data BitcoinMonadException
  = UnableToSerializeAddress Address NetworkId
  | UnableToChooseCoins
      -- | Transaction skeleton.
      TxSkeleton
      -- | Available UTxOs.
      [UTxO]
      -- | Total required output value.
      Satoshi
      -- | Fee rate.
      Satoshi
      -- | Error message given by coin selection.
      String
  | UnableToSignTx
      -- | Transaction.
      Tx
      -- | Error message given by Haskoin.
      String
  | WaitForTxConfirmationsTimeout
      -- | Transaction hash.
      TxHash
      -- | Wait configuration.
      TxConfirmationsConfig
      -- | Last known confirmation count.
      (Maybe BlockHeight)
  deriving stock (Show)
  deriving anyclass (Exception)