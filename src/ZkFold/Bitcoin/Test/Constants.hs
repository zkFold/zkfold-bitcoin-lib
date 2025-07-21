module ZkFold.Bitcoin.Test.Constants (
  testWalletAddress,
  testWalletXPrvKey,
  testWalletXPubKey,
  testWalletMasterXPrvKey,
  testWalletMasterXPubKey,
  exportedTestWalletMasterXPrvKey,
  exportedTestWalletMasterXPubKey,
) where

import Data.Function ((&))
import GHC.IO (unsafePerformIO)
import Haskoin (Address, Base58, Mnemonic, Seed, XPrvKey, XPubKey, btcRegTest, deriveXPubKey, makeXPrvKey, mnemonicToSeed, prvSubKey, pubSubKey, withContext, xPrvExport, xPubExport, xPubWitnessAddr)

mnemonic :: Mnemonic
mnemonic = "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote"

mnemonicSeed :: Seed
mnemonicSeed = mnemonicToSeed "" mnemonic & either error id

testWalletMasterXPrvKey :: XPrvKey
testWalletMasterXPrvKey = makeXPrvKey mnemonicSeed

exportedTestWalletMasterXPrvKey :: Base58
exportedTestWalletMasterXPrvKey = testWalletMasterXPrvKey & xPrvExport btcRegTest

{-# NOINLINE testWalletMasterXPubKey #-}
testWalletMasterXPubKey :: XPubKey
testWalletMasterXPubKey = unsafePerformIO $ withContext $ \ctx -> pure $ deriveXPubKey ctx testWalletMasterXPrvKey

{-# NOINLINE exportedTestWalletMasterXPubKey #-}
exportedTestWalletMasterXPubKey :: Base58
exportedTestWalletMasterXPubKey = unsafePerformIO $ withContext $ \ctx -> pure $ xPubExport btcRegTest ctx testWalletMasterXPubKey

{-# NOINLINE testWalletXPrvKey #-}
testWalletXPrvKey :: XPrvKey
testWalletXPrvKey = unsafePerformIO $ withContext $ \ctx -> pure $ prvSubKey ctx testWalletMasterXPrvKey 0

{-# NOINLINE testWalletXPubKey #-}
testWalletXPubKey :: XPubKey
testWalletXPubKey = unsafePerformIO $ withContext $ \ctx -> pure $ pubSubKey ctx testWalletMasterXPubKey 0

{-# NOINLINE testWalletAddress #-}
testWalletAddress :: Address
testWalletAddress = unsafePerformIO $ withContext $ \ctx -> pure $ xPubWitnessAddr ctx $ pubSubKey ctx testWalletMasterXPubKey 0

-- TODO: Get rid of unsafePerformIO and expose something like mkSetup.