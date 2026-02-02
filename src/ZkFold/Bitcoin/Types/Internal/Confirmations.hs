module ZkFold.Bitcoin.Types.Internal.Confirmations (
  TxConfirmationsConfig (..),
  defaultTxConfirmationsConfig,
  pollIntervalMicros,
) where

import GHC.Natural (Natural)
import ZkFold.Bitcoin.Types.Internal.BlockHeight (BlockHeight)

-- | Configuration for waiting on transaction confirmations.
data TxConfirmationsConfig = TxConfirmationsConfig
  { tccConfirmations :: BlockHeight
  , tccPollIntervalSeconds :: Int
  , tccMaxAttempts :: Natural
  }
  deriving stock (Show, Eq)

-- | Default config: 1 confirmation, 10s interval, 60 attempts.
defaultTxConfirmationsConfig :: TxConfirmationsConfig
defaultTxConfirmationsConfig =
  TxConfirmationsConfig
    { tccConfirmations = 1
    , tccPollIntervalSeconds = 10
    , tccMaxAttempts = 60
    }

pollIntervalMicros :: TxConfirmationsConfig -> Int
pollIntervalMicros config =
  max 0 (tccPollIntervalSeconds config) * 1_000_000
