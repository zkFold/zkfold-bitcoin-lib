module ZkFold.Bitcoin.Types.Internal.Common (
  isHexString,
) where

import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.Text qualified as Text

-- | Check if a text is a valid hex string.
isHexString :: Text -> Bool
isHexString = Text.all isHexDigit