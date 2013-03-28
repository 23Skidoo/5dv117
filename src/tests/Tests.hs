module Main
       where

import Control.Monad (replicateM)
import Test.Framework as TF
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 as TF

import CFG.Examples
import CFG.Parse

-- QuickCheck test.
prop_parentheses :: Gen Bool
prop_parentheses = do
  m <- choose (1,5)
  input <- replicateM m $ do
    n <- choose (1,10)
    return $! replicate n '(' ++ replicate n ')'
  return $! cykAlgorithm balancedParentheses (concat input)

allTests :: [TF.Test]
allTests = [
  testProperty "prop_parentheses" prop_parentheses
  ]

main :: IO ()
main = defaultMain allTests
