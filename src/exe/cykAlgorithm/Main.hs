module Main
       where

import System.Environment (getArgs)

import CFG.Examples
import CFG.Helpers.CNF    (compileGrammar)
import CFG.Parse
import CFG.Read

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
-- $ cykAlgorithm "S -> SS, S -> LH, S -> LR, H -> SR, L -> (, R -> )" "(())()"
-- Recognised.
-- $ cykAlgorithm "S -> SS, S -> LH, S -> LR, H -> SR, L -> (, R -> )" "zZz"
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
