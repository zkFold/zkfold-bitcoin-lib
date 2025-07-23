{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Bitcoin.Test.RegTest (regTestTests) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Haskoin (SigInput (..), addressToOutput, sigHashAll, signTx, withContext)
import Haskoin.Crypto.Keys.Extended qualified as H
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Bitcoin.Class (BitcoinBuilderMonad (..), BitcoinQueryMonad (networkId, submitTx, utxosAtAddress))
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
        network <- runBitcoinQueryMonadIO provider $ networkFromId <$> networkId
        step $ "testWalletUTxOs: " <> show testWalletUTxOs
        withContext $ \ctx -> do
          let outToSend = btcToSatoshi 10
              addr2Output = addressToOutput testWalletAddress2
              txSkel = mustHaveOutput (addr2Output, outToSend)
          (tx, selectIns) <- runBitcoinBuilderMonadIO provider [testWalletAddress] testWalletAddress $ buildTx txSkel
          step $ "tx: " <> show tx
          let signedTx = signTx network ctx tx (selectIns <&> (\selectIn -> SigInput (addressToOutput testWalletAddress) (selectIn & utxoValue) (selectIn & utxoOutpoint) sigHashAll Nothing)) [testWalletXPrvKey.key] & either error id
          step $ "signedTx: " <> show signedTx
          txid <- runBitcoinQueryMonadIO provider $ submitTx signedTx
          step $ "txid: " <> show txid
    ]