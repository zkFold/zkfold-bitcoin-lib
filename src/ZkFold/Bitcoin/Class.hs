module ZkFold.Bitcoin.Class (
  BitcoinQueryMonad (..),
) where

import Data.Text (Text)
import Haskoin (Tx, TxHash)
import ZkFold.Bitcoin.Types

class BitcoinQueryMonad m where
  {-# MINIMAL blockCount, bestBlockHash, blockHeader, blockHash, utxosAtAddress, submitTx, networkId #-}

  -- | Get the height of the most-work fully-validated chain.
  blockCount :: m BlockHeight

  -- | Get the 'BlockHash' of the best block.
  bestBlockHash :: m BlockHash

  -- | Get the 'BlockHeader' of a given 'BlockHash'.
  blockHeader :: BlockHash -> m BlockHeader

  -- | Get the 'BlockHash' of a given block height.
  blockHash :: BlockHeight -> m BlockHash

  -- TODO: Need a type for end address.

  -- | Get the 'UTxO's at a given address.
  --
  -- Note that we'll return UTxOs irrespective of number of confirmations. This can be problematic for coinbase UTxOs since they require 100 confirmations.
  utxosAtAddress :: Text -> m [UTxO]

  -- | Submit (broadcast) a transaction to the Bitcoin network.
  submitTx :: Tx -> m TxHash

  -- | Get the 'NetworkId' of the Bitcoin network.
  networkId :: m NetworkId
