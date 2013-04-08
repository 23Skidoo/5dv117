-- | The CYK algorithm
module CFG.Parse (cykAlgorithm)
       where

import Control.Monad
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST

import CFG.Types
import CFG.CNFHelpers

-- The CYK algorithm.
cykAlgorithm :: CompiledCNFGrammar -> String -> Bool
cykAlgorithm grammar input' = if n == 0
                              then cnfProducesEmpty grammar
                              else or [arr ! (1,n,x) | x <- startIndices]
  where
    n = length input
    r = length rules
    rules        = cnfRules grammar
    startIndices = map ruleNumber . filter (isStartRule grammar) $ rules
    input        = stringToSymbols input'

    arr = runSTUArray $ do
      marr <- newArray ((1,1,1),(n,n,r)) False

      forM_ (zip [1..] input) $ \(i, ci) ->
        forM_ (filter isTerminalRule rules) $ \rule -> do
          let j = ruleNumber rule
          when (terminalRuleProduces rule ci) $
            writeArray marr (i,1,j) True

      forM_ [2..n] $ \i ->
        forM_ [1..(n-i+1)] $ \j ->
          forM_ [1..(i-1)] $ \k ->
            forM_ (filter isNonTerminalRule rules) $ \rule -> do
              let a = ruleNumber rule
              forM_ (nonTerminalRuleProductions rule) $ \(b,c) -> do
                e0 <- readArray marr (j,k,b)
                e1 <- readArray marr (j+k,i-k,c)
                when (e0 && e1) $
                  writeArray marr (j,i,a) True
      return marr
