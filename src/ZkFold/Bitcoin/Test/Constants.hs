module ZkFold.Bitcoin.Test.Constants (
  testWalletAddress,
  testWalletAddress2,
  testWalletXPrvKey,
  testWalletXPrvKey2,
  testWalletXPubKey,
  testWalletXPubKey2,
  testWalletMasterXPrvKey,
  testWalletMasterXPubKey,
  testWalletMasterXPubKey2,
  exportedTestWalletMasterXPrvKey,
  exportedTestWalletMasterXPrvKey2,
  exportedTestWalletMasterXPubKey,
  exportedTestWalletMasterXPubKey2,
) where

import Data.Function ((&))
import GHC.IO (unsafePerformIO)
import Haskoin (Address, Base58, Mnemonic, Seed, XPrvKey, XPubKey, btcRegTest, deriveXPubKey, makeXPrvKey, mnemonicToSeed, prvSubKey, pubSubKey, withContext, xPrvExport, xPubExport, xPubWitnessAddr)

mnemonic :: Mnemonic
mnemonic = "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote"

mnemonic2 :: Mnemonic
mnemonic2 = "hamster diagram private dutch cause delay private meat slide toddler razor book happy fancy gospel tennis maple dilemma loan word shrug inflict delay length"

mnemonicSeed :: Seed
mnemonicSeed = mnemonicToSeed "" mnemonic & either error id

mnemonicSeed2 :: Seed
mnemonicSeed2 = mnemonicToSeed "" mnemonic2 & either error id

testWalletMasterXPrvKey :: XPrvKey
testWalletMasterXPrvKey = makeXPrvKey mnemonicSeed

testWalletMasterXPrvKey2 :: XPrvKey
testWalletMasterXPrvKey2 = makeXPrvKey mnemonicSeed2

exportedTestWalletMasterXPrvKey :: Base58
exportedTestWalletMasterXPrvKey = testWalletMasterXPrvKey & xPrvExport btcRegTest

exportedTestWalletMasterXPrvKey2 :: Base58
exportedTestWalletMasterXPrvKey2 = testWalletMasterXPrvKey2 & xPrvExport btcRegTest

{-# NOINLINE testWalletMasterXPubKey #-}
testWalletMasterXPubKey :: XPubKey
testWalletMasterXPubKey = unsafePerformIO $ withContext $ \ctx -> pure $ deriveXPubKey ctx testWalletMasterXPrvKey

{-# NOINLINE testWalletMasterXPubKey2 #-}
testWalletMasterXPubKey2 :: XPubKey
testWalletMasterXPubKey2 = unsafePerformIO $ withContext $ \ctx -> pure $ deriveXPubKey ctx testWalletMasterXPrvKey2

{-# NOINLINE exportedTestWalletMasterXPubKey #-}
exportedTestWalletMasterXPubKey :: Base58
exportedTestWalletMasterXPubKey = unsafePerformIO $ withContext $ \ctx -> pure $ xPubExport btcRegTest ctx testWalletMasterXPubKey

{-# NOINLINE exportedTestWalletMasterXPubKey2 #-}
exportedTestWalletMasterXPubKey2 :: Base58
exportedTestWalletMasterXPubKey2 = unsafePerformIO $ withContext $ \ctx -> pure $ xPubExport btcRegTest ctx testWalletMasterXPubKey2

{-# NOINLINE testWalletXPrvKey #-}
testWalletXPrvKey :: XPrvKey
testWalletXPrvKey = unsafePerformIO $ withContext $ \ctx -> pure $ prvSubKey ctx testWalletMasterXPrvKey 0

{-# NOINLINE testWalletXPrvKey2 #-}
testWalletXPrvKey2 :: XPrvKey
testWalletXPrvKey2 = unsafePerformIO $ withContext $ \ctx -> pure $ prvSubKey ctx testWalletMasterXPrvKey2 0

{-# NOINLINE testWalletXPubKey #-}
testWalletXPubKey :: XPubKey
testWalletXPubKey = unsafePerformIO $ withContext $ \ctx -> pure $ pubSubKey ctx testWalletMasterXPubKey 0

{-# NOINLINE testWalletXPubKey2 #-}
testWalletXPubKey2 :: XPubKey
testWalletXPubKey2 = unsafePerformIO $ withContext $ \ctx -> pure $ pubSubKey ctx testWalletMasterXPubKey2 0

{-# NOINLINE testWalletAddress #-}
testWalletAddress :: Address
testWalletAddress = unsafePerformIO $ withContext $ \ctx -> pure $ xPubWitnessAddr ctx $ pubSubKey ctx testWalletMasterXPubKey 0

{-# NOINLINE testWalletAddress2 #-}
testWalletAddress2 :: Address
testWalletAddress2 = unsafePerformIO $ withContext $ \ctx -> pure $ xPubWitnessAddr ctx $ pubSubKey ctx testWalletMasterXPubKey2 0

-- TODO: Get rid of unsafePerformIO and expose something like mkSetup.
-- TODO: Likely create a setup with multiple test wallets (under something like list)