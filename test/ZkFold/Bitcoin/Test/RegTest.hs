{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Haskoin (addressToOutput)
import Haskoin.Crypto.Keys.Extended qualified as H
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (submitTx, utxosAtAddress), signTx)
import ZkFold.Bitcoin.IO
import ZkFold.Bitcoin.Test.Constants (testWalletAddress, testWalletAddress2, testWalletXPrvKey)
import ZkFold.Bitcoin.Types
import ZkFold.Bitcoin.Types.Internal.Common (btcToSatoshi)
import ZkFold.Bitcoin.Types.Internal.Skeleton

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
        let txSkel = mustHaveOutput (addressToOutput testWalletAddress2, btcToSatoshi 10)
        (tx, selectIns) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ buildTx txSkel
        step $ "tx: " <> show tx
        signedTx <- runBitcoinSignerMonadIO provider [testWalletAddress] testWalletAddress [testWalletXPrvKey.key] $ signTx (tx, selectIns)
        step $ "signedTx: " <> show signedTx
        txid <- runBitcoinQueryMonadIO provider $ submitTx signedTx
        step $ "txid: " <> show txid
    ]