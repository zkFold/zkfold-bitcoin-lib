module ZkFold.Bitcoin.Types.Internal.Common (
  isHexString,
) where

import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.Text qualified as Text

isHexString :: Text -> Bool
isHexString = Text.all isHexDigit