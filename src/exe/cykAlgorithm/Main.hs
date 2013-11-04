module Main
       where

import System.Environment (getArgs)

import CFG.Examples
import CFG.Parse
import CFG.Read
import CFG.Types

exampleValidInput :: String
exampleValidInput = "((((()))))"

exampleInvalidInput :: String
exampleInvalidInput = "(()"

demo :: IO ()
demo = do
  putStrLn "Demo: balanced parentheses"
  putStrLn $ "Valid input: " ++ exampleValidInput
  let validResult = cykAlgorithm balancedParentheses exampleValidInput
  putStrLn $ "Result for the valid input: " ++ (show validResult)
  putStrLn $ "Invalid input: " ++ exampleInvalidInput
  let invalidResult = cykAlgorithm balancedParentheses exampleInvalidInput
  putStrLn $ "Result for the invalid input: " ++ (show invalidResult)

-- | Usage:
-- $ cykAlgorithm "S -> AB, A -> a, B -> B1B, B -> B1B1, B1 -> b" "abbb"
-- Recognised.
-- $ cykAlgorithm "S -> AB, A -> a, B -> B1B, B -> B1B1, B1 -> b" "zZz"
-- Not recognised.
main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then demo
    else do
    let inp0 = args !! 0
        inp1 = args !! 1
    case readCNFGrammar inp0 of
      Left err -> error err
      Right gr -> let grammar = compileGrammar gr
                      res = cykAlgorithm grammar inp1
                  in putStrLn $ (if res then "Recognised."
                                 else "Not recognised.")
