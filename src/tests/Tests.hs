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

assertRecognised :: String -> CompiledCNFGrammar -> String -> Assertion
assertRecognised grammarName grammar str =
  assertBool ("String '" ++ str ++ "' not recognised by the '"
              ++ grammarName ++ "' grammar.")
  (cykAlgorithm grammar str)

assertNotRecognised :: String -> CompiledCNFGrammar -> String -> Assertion
assertNotRecognised grammarName grammar str =
  assertBool ("String '" ++ str ++ "' erroneously recognised by the '"
              ++ grammarName ++ "' grammar.")
  (not $ cykAlgorithm grammar str)

test_silly :: Assertion
test_silly = do
  assertNotRecognised "silly1" sillyGrammar "o"
  assertRecognised    "silly1" sillyGrammar "oo"
  assertNotRecognised "silly1" sillyGrammar "ooo"
  assertNotRecognised "silly1" sillyGrammar "not silly"

test_silly2 :: Assertion
test_silly2 = do
  assertRecognised    "silly2" sillyGrammar2 "lo"
  assertRecognised    "silly2" sillyGrammar2 "ol"
  assertRecognised    "silly2" sillyGrammar2 "olol"
  assertRecognised    "silly2" sillyGrammar2 "lolo"
  assertNotRecognised "silly2" sillyGrammar2 "ololl"

test_read_cnf :: Assertion
test_read_cnf = do
  let e = readCNFGrammar $ "S -> S1S2, S1 -> o, S2 -> o"
  case e of
    (Left err) -> assertFailure $ "Parse failed: " ++ err
    (Right gr) -> do
      let gram = compileGrammar gr
      assertRecognised    "myGrammar" gram "oo"
      assertNotRecognised "myGrammar" gram ""
      assertNotRecognised "myGrammar" gram "erroneous"

test_read_cnf_empty :: Assertion
test_read_cnf_empty = do
  let e = readCNFGrammar $ "S ->, S -> S1S2, S1 -> o, S2 -> o"
  case e of
    (Left err) -> assertFailure $ "Parse failed: " ++ err
    (Right gr) -> do
      let gram = compileGrammar gr
      assertRecognised    "myGrammar" gram ""
      assertRecognised    "myGrammar" gram "oo"
      assertNotRecognised "myGrammar" gram "erroneous"

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
      , testCase "test_read_cnf" test_read_cnf
      , testCase "test_read_cnf_empty" test_read_cnf_empty
      ]
