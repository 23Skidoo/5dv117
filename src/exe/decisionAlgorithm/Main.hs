module Main
       where

import System.Environment (getArgs)

demo :: IO ()
demo = putStrLn "Not implemented."

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then demo
    else putStrLn "Not implemented."
