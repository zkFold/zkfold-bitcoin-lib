module ZkFold.Bitcoin.Class (
  BitcoinQueryMonad (..),
  BitcoinBuilderQueryMonad (..),
) where

import Haskoin (Address)
import ZkFold.Bitcoin.Types

class BitcoinQueryMonad m where
  {-# MINIMAL blockCount, bestBlockHash, blockHeader, blockHash #-}

  -- | Get the height of the most-work fully-validated chain.
  blockCount :: m BlockHeight

  -- | Get the 'BlockHash' of the best block.
  bestBlockHash :: m BlockHash

  -- | Get the 'BlockHeader' of a given 'BlockHash'.
  blockHeader :: BlockHash -> m BlockHeader

  -- | Get the 'BlockHash' of a given block height.
  blockHash :: BlockHeight -> m BlockHash

class BitcoinBuilderQueryMonad m where
  {-# MINIMAL utxosAtAddress #-}
  utxosAtAddress :: Address -> m [UTxO]