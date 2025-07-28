module ZkFold.Bitcoin.Utils (
  wordToScriptOp,
) where

import Data.ByteString qualified as BS
import Data.Bytes.Put (MonadPut (..), runPutS)
import Data.Word (Word32, Word8)
import Haskoin (ScriptOp (..), opPushData)
import Haskoin qualified

{- | Convert a number to the appropriate @ScriptO@.
For numbers 0-16, use the built-in opcodes.
For larger numbers, use @OP_PUSHDATA@ with the encoded bytes.
-}
wordToScriptOp :: Word32 -> ScriptOp
wordToScriptOp n
  | n == 0 = OP_0
  | n <= 16 = Haskoin.intToScriptOp (fromIntegral n)
  | otherwise = opPushData (encodeNumberBytes n)

encodeNumberBytes :: Word32 -> BS.ByteString
encodeNumberBytes n = runPutS $ do
  mapM_ putWord8 (encodeWord32 n)

{- | See following resources for valid encoding:
1. https://bitcoin.stackexchange.com/a/122944
2. https://github.com/bitcoinjs/bitcoinjs-lib/blob/248789d25b9833ed286c9ca4b9bfd93f099fd8a3/ts_src/script_number.ts#L71
-}
encodeWord32 :: Word32 -> [Word8]
encodeWord32 n =
  reverse $
    if toBytes == mempty
      then toBytes
      else
        if head toBytes >= 0x80
          then 0x00 : toBytes
          else toBytes
 where
  toBytes = toBytes' n
  toBytes' :: Word32 -> [Word8]
  toBytes' 0 = []
  toBytes' x = fromIntegral (x `mod` 256) : toBytes' (x `div` 256)