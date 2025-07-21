module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinQueryMonad (utxosAtAddress))
import ZkFold.Bitcoin.IO (runBitcoinQueryMonadIO)
import ZkFold.Bitcoin.Test.Constants (testWalletAddress)
import ZkFold.Bitcoin.Types

regTestTests :: TestTree
regTestTests =
  testGroup
    "regtest"
    [ testCaseSteps "simple send" $ \step -> do
        let providerConfig =
              BPCNode
                ( BitcoinProviderConfigNode
                    { bpcnUsername = "user"
                    , bpcnPassword = "password"
                    , bpcnUrl = "http://localhost:18443"
                    , bpcnNetworkId = RegTest
                    }
                )
        provider <- providerFromConfig providerConfig
        testWalletUTxOs <- runBitcoinQueryMonadIO provider $ do
          utxosAtAddress testWalletAddress
        step $ "testWalletUTxOs: " <> show testWalletUTxOs
    ]