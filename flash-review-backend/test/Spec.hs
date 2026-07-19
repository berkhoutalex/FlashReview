module Main where

import qualified MainSpec
import           Test.Hspec

main :: IO ()
main = hspec MainSpec.spec
