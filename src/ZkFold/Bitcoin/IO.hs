module ZkFold.Bitcoin.IO (
  BitcoinQueryMonadIO,
  runBitcoinQueryMonadIO,
  BitcoinBuilderMonadIO,
  runBitcoinBuilderMonadIO,
) where

import Control.Arrow ((>>>))
import Control.Exception (catch, throwIO)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Monoid (Sum (..))
import Haskoin (Address, addressToOutput, chooseCoins, withContext)
import Haskoin qualified
import ZkFold.Bitcoin.Class
import ZkFold.Bitcoin.Errors
import ZkFold.Bitcoin.Types (BitcoinProvider (..), UTxO (..))
import ZkFold.Bitcoin.Types.Internal.Skeleton

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

instance MonadError BitcoinMonadException BitcoinQueryMonadIO where
  throwError = ioToBitcoinQueryMonadIO . throwIO
  catchError action handler = do
    env <- ask
    ioToBitcoinQueryMonadIO $
      catch
        (runBitcoinQueryMonadIO' action env)
        (\err -> handler err `runBitcoinQueryMonadIO'` env)

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

type role BitcoinBuilderMonadIO representational

data BitcoinBuilderIOEnv = BitcoinBuilderIOEnv
  { builderEnvAddresses :: [Address]
  , builderEnvChangeAddress :: Address
  }

newtype BitcoinBuilderMonadIO a = BitcoinBuilderMonadIO {runBitcoinBuilderMonadIO' :: BitcoinBuilderIOEnv -> BitcoinQueryMonadIO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader BitcoinBuilderIOEnv
    , MonadError BitcoinMonadException
    , BitcoinQueryMonad
    )
    via ReaderT BitcoinBuilderIOEnv BitcoinQueryMonadIO

ioToBitcoinBuilderMonadIO :: IO a -> BitcoinBuilderMonadIO a
ioToBitcoinBuilderMonadIO = BitcoinBuilderMonadIO . const . ioToBitcoinQueryMonadIO

instance BitcoinBuilderMonad BitcoinBuilderMonadIO where
  buildTx skel = do
    BitcoinBuilderIOEnv{..} <- ask
    utxosWithAddr <-
      mapM
        ( \addr ->
            do
              utxos <- utxosAtAddress addr
              pure (utxos, addr)
        )
        builderEnvAddresses
    let allUtxos = map fst utxosWithAddr & mconcat
        totalOutSats = txSkelOuts skel & foldMap (snd >>> Sum) & getSum
        -- TODO: Fetch it from network.
        feePerByte = 1
    (selectIns, change) <- case chooseCoins totalOutSats feePerByte (length (txSkelOuts skel) + 1) True allUtxos of
      Left err -> throwError $ UnableToChooseCoins allUtxos totalOutSats feePerByte err
      Right res -> pure res
    ioToBitcoinBuilderMonadIO $ withContext $ \ctx -> do
      let tx = Haskoin.buildTx ctx (selectIns <&> utxoOutpoint) (txSkelOuts skel <> [(addressToOutput builderEnvChangeAddress, change)])
      pure (tx, selectIns)

runBitcoinBuilderMonadIO :: BitcoinProvider -> [Address] -> Address -> BitcoinBuilderMonadIO a -> IO a
runBitcoinBuilderMonadIO provider addresses changeAddress act =
  runBitcoinQueryMonadIO
    provider
    ( runBitcoinBuilderMonadIO'
        act
        (BitcoinBuilderIOEnv{builderEnvAddresses = addresses, builderEnvChangeAddress = changeAddress})
    )