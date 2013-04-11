module Main
       where

import Control.Monad                        (replicateM)
import Test.Framework                       as TF
import Test.Framework.Providers.HUnit       as TF
import Test.Framework.Providers.QuickCheck2 as TF
import Test.HUnit
import Test.QuickCheck

import CFG.Examples
import CFG.Parse
import CFG.Read
import CFG.Types

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
  let res1 = cykAlgorithm sillyGrammar "oo"
      res2 = cykAlgorithm sillyGrammar "not silly"
  assertBool "Silly string not recognised by an silly-producing grammar." res1
  assertBool "Silly-producing grammar recognised a non-silly string." (not res2)

test_silly2 :: Assertion
test_silly2 = do
  let res1 = cykAlgorithm sillyGrammar2 "10"
      res2 = cykAlgorithm sillyGrammar2 "01"
      res3 = cykAlgorithm sillyGrammar2 "0101"
      res4 = cykAlgorithm sillyGrammar2 "1010"
      res5 = cykAlgorithm sillyGrammar2 "01011"
  assertBool "Silly string 1 not recognised by an silly-producing grammar." res1
  assertBool "Silly string 2 not recognised by an silly-producing grammar." res2
  assertBool "Silly string 3 not recognised by an silly-producing grammar." res3
  assertBool "Silly string 4 not recognised by an silly-producing grammar." res4
  assertBool "Silly-producing grammar recognised a non-silly string." (not res5)

test_read :: Assertion
test_read = do
  let e = readCNFGrammar $ "S -> S1S2, S1 -> o, S2 -> o"
  case e of
    (Left err) -> assertFailure $ "Parse failed: " ++ err
    (Right gr) -> do
      let gram = compileGrammar gr
          res1 = cykAlgorithm gram "oo"
          res2 = cykAlgorithm gram "erroneous"
      assertBool "String not recognised by 'myGrammar'." res1
      assertBool "'myGrammar' recognised an invalid string." (not res2)


-- Entry point
main :: IO ()
main = defaultMain allTests
  where
    allTests :: [TF.Test]
    allTests =
      [ testProperty "prop_parentheses" prop_parentheses
      , testProperty "prop_parentheses_inverted" prop_parentheses_inverted
      , testCase "test_silly" test_silly
      , testCase "test_silly2" test_silly2
      , testCase "test_read" test_read
      ]
