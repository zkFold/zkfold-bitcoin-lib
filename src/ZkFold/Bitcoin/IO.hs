module ZkFold.Bitcoin.IO (
  BitcoinQueryMonadIO,
  runBitcoinQueryMonadIO,
) where

import Control.Monad.Reader
import ZkFold.Bitcoin.Class
import ZkFold.Bitcoin.Types (BitcoinProvider (..))

type role BitcoinQueryMonadIO representational

-- This is not simply a wrapper around 'ReaderT BitcoinProvider IO' because its type parameter has role set to @nominal@.
newtype BitcoinQueryMonadIO a = BitcoinQueryMonadIO {runBitcoinQueryMonadIO' :: BitcoinProvider -> IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader BitcoinProvider
    )
    via ReaderT BitcoinProvider IO

instance BitcoinQueryMonad BitcoinQueryMonadIO where
  blockCount = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ bpBlockCount provider

  bestBlockHash = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ bpBestBlockHash provider

  blockHeader givenBlockHash = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ bpBlockHeader provider givenBlockHash

  blockHash height = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ bpBlockHash provider height

  utxosAtAddress addr = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ bpUtxosAtAddress provider addr

  submitTx tx = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ bpSubmitTx provider tx

  networkId = do
    provider <- ask
    ioToBitcoinQueryMonadIO $ pure $ bpNetworkId provider

{- | INTERNAL USAGE ONLY

Do not expose a 'MonadIO' instance for 'BitcoinQueryMonadIO', as it is not safe to run arbitrary IO actions in the context of a 'BitcoinQueryMonadIO' action.

Note that constructor of 'BitcoinQueryMonadIO' is not exported so user cannot construct 'BitcoinQueryMonadIO' containing arbitrary IO actions.
-}
ioToBitcoinQueryMonadIO :: IO a -> BitcoinQueryMonadIO a
ioToBitcoinQueryMonadIO = BitcoinQueryMonadIO . const

runBitcoinQueryMonadIO :: BitcoinProvider -> BitcoinQueryMonadIO a -> IO a
runBitcoinQueryMonadIO = flip runBitcoinQueryMonadIO'