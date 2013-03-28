module Main
       where

import CFG.Examples
import CFG.Parse

exampleValidInput :: String
exampleValidInput = "((((()))))"

exampleInvalidInput :: String
exampleInvalidInput = "(()"

main :: IO ()
main = do
  let validResult = cykAlgorithm balancedParentheses exampleValidInput
  putStrLn $ "Result for the valid input: " ++ (show validResult)
  let invalidResult = cykAlgorithm balancedParentheses exampleInvalidInput
  putStrLn $ "Result for the invalid input: " ++ (show invalidResult)
