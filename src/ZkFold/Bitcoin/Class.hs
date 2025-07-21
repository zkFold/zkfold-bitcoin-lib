module ZkFold.Bitcoin.Class (
  BitcoinQueryMonad (..),
) where

import Control.Monad.Error.Class (MonadError)
import Haskoin (Address, Tx, TxHash)
import ZkFold.Bitcoin.Errors (BitcoinQueryMonadException)
import ZkFold.Bitcoin.Types

class (MonadError BitcoinQueryMonadException m) => BitcoinQueryMonad m where
  {-# MINIMAL blockCount, bestBlockHash, blockHeader, blockHash, utxosAtAddress, submitTx, networkId #-}

  -- | Get the height of the most-work fully-validated chain.
  blockCount :: m BlockHeight

  -- | Get the 'BlockHash' of the best block.
  bestBlockHash :: m BlockHash

  -- | Get the 'BlockHeader' of a given 'BlockHash'.
  blockHeader :: BlockHash -> m BlockHeader

  -- | Get the 'BlockHash' of a given block height.
  blockHash :: BlockHeight -> m BlockHash

  -- | Get the 'UTxO's at a given address.
  --
  -- Note that we'll return UTxOs irrespective of number of confirmations. This can be problematic for coinbase UTxOs since they require 100 confirmations.
  utxosAtAddress :: Address -> m [UTxO]

  -- | Submit (broadcast) a transaction to the Bitcoin network.
  submitTx :: Tx -> m TxHash

  -- | Get the 'NetworkId' of the Bitcoin network.
  networkId :: m NetworkId
