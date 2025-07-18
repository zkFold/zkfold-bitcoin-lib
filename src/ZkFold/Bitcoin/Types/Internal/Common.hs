module ZkFold.Bitcoin.Types.Internal.Common (
  LowerFirst,
) where

import Data.Char (toLower)
import Deriving.Aeson

data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier "" = ""
  getStringModifier (c : cs) = toLower c : cs