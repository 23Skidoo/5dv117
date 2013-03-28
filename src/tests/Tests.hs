module Main
       where

import Control.Monad (replicateM)
import Test.Framework as TF
import Test.Framework.Providers.HUnit as TF
import Test.Framework.Providers.QuickCheck2 as TF
import Test.HUnit
import Test.QuickCheck

import CFG.Examples
import CFG.Parse

-- QuickCheck tests.
prop_parentheses_gen :: Bool -> Gen Bool
prop_parentheses_gen invert = do
  m <- choose (1,5)
  input <- replicateM m $ do
    n <- choose (1,10)
    return $! replicate n '(' ++ (if invert then [] else replicate n ')')
  return $! (if invert then not else id)
    $ cykAlgorithm balancedParentheses (concat input)

prop_parentheses :: Gen Bool
prop_parentheses = prop_parentheses_gen False

prop_parentheses_inverted :: Gen Bool
prop_parentheses_inverted = prop_parentheses_gen True

-- HUnit tests.

test_silly :: Assertion
test_silly = do
  let res1 = cykAlgorithm sillyGrammar "OO"
  let res2 = cykAlgorithm sillyGrammar "NOT SILLY"
  assertBool "Silly string not recognised by an silly-producing grammar." res1
  assertBool "Silly-producing grammar recognised a non-silly string." (not res2)

-- Entry point
main :: IO ()
main = defaultMain allTests
  where
    allTests :: [TF.Test]
    allTests =
      [ testProperty "prop_parentheses" prop_parentheses
      , testProperty "prop_parentheses_inverted" prop_parentheses_inverted
      , testCase "test_silly" test_silly
      ]
