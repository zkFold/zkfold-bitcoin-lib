module ZkFold.Bitcoin.Class (BitcoinQueryMonad (..)) where

import GHC.Natural (Natural)
import ZkFold.Bitcoin.Types

class BitcoinQueryMonad m where
  {-# MINIMAL blockCount, bestBlockHash, blockHeader, blockHash #-}

  -- | Get the height of the most-work fully-validated chain.
  blockCount :: m Natural

  -- | Get the 'BlockHash' of the best block.
  bestBlockHash :: m BlockHash

  -- | Get the 'BlockHeader' of a given 'BlockHash'.
  blockHeader :: BlockHash -> m BlockHeader

  -- | Get the 'BlockHash' of a given block height.
  blockHash :: Natural -> m BlockHash