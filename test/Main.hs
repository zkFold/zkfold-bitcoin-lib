module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import ZkFold.Bitcoin.Test.RegTest (regTestTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "zkfold-bitcoin-lib"
      [ regTestTests
      ]
