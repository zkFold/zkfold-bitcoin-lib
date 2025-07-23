module ZkFold.Bitcoin.Class (
  BitcoinQueryMonad (..),
  BitcoinBuilderMonad (..),
  BitcoinSignerMonad (..),
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader (ReaderT, lift)
import Haskoin (Address, Tx, TxHash)
import ZkFold.Bitcoin.Errors (BitcoinMonadException)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (Satoshi)
import ZkFold.Bitcoin.Types.Internal.Skeleton (TxSkeleton)

class (MonadError BitcoinMonadException m) => BitcoinQueryMonad m where
  {-# MINIMAL blockCount, bestBlockHash, blockHeader, blockHash, utxosAtAddress, submitTx, networkId, recommendedFeeRate #-}

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

  -- TODO: Give more options like number of blocks to confirm in, etc.?

  -- | Get the recommended fee rate in satoshi per virtual byte of the network for the transaction to get quickly confirmed (i.e. in 1 block).
  recommendedFeeRate :: m Satoshi

instance (BitcoinQueryMonad m) => BitcoinQueryMonad (ReaderT r m) where
  blockCount = lift blockCount
  bestBlockHash = lift bestBlockHash
  blockHeader = lift . blockHeader
  blockHash = lift . blockHash
  utxosAtAddress = lift . utxosAtAddress
  submitTx = lift . submitTx
  networkId = lift networkId
  recommendedFeeRate = lift recommendedFeeRate

class (BitcoinQueryMonad m) => BitcoinBuilderMonad m where
  {-# MINIMAL buildTx #-}

  -- | Build a transaction from a 'TxSkeleton', returning the transaction and the UTxOs that are being spent by this transaction.
  buildTx :: TxSkeleton -> m (Tx, [UTxO])

instance (BitcoinBuilderMonad m) => BitcoinBuilderMonad (ReaderT r m) where
  buildTx = lift . buildTx

class (BitcoinBuilderMonad m) => BitcoinSignerMonad m where
  {-# MINIMAL signTx #-}

  -- | Sign a transaction with given UTxOs that are being spent by it.
  signTx :: (Tx, [UTxO]) -> m Tx