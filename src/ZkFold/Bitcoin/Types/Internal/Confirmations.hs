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
  , tccPollIntervalSeconds :: Natural
  , tccMaxAttempts :: Natural
  }
  deriving stock (Show, Eq)

-- | Default config: 1 confirmation, 30s interval, 60 attempts.
defaultTxConfirmationsConfig :: TxConfirmationsConfig
defaultTxConfirmationsConfig =
  TxConfirmationsConfig
    { tccConfirmations = 1
    , tccPollIntervalSeconds = 30
    , tccMaxAttempts = 60
    }

pollIntervalMicros :: TxConfirmationsConfig -> Int
pollIntervalMicros config =
  fromIntegral (tccPollIntervalSeconds config) * 1_000_000
