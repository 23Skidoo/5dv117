module CYK
       where

import Control.Monad
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Maybe
import Test.QuickCheck
import qualified Data.Map as M

-- Grammar ADT definition.
type Symbol     = Char
type RuleName   = String
type RuleNumber = Int

data IsStartRule = StartRule | NormalRule
                 deriving (Eq,Show)
data CNFRule a = TerminalRule a Symbol
               | NonTerminalRule a [(a, a)] IsStartRule
               deriving (Eq, Show)

type NamedCNFRule    = CNFRule RuleName
type NumberedCNFRule = CNFRule RuleNumber

type NamedCNFGrammar = [NamedCNFRule]
type CNFGrammar = [NumberedCNFRule]

-- Helpers for constructing the grammar.

ruleTerminal :: RuleName -> Symbol -> NamedCNFRule
ruleTerminal name prod = TerminalRule name prod

ruleNonTerminal :: RuleName -> [(RuleName, RuleName)] -> NamedCNFRule
ruleNonTerminal name prods = NonTerminalRule name prods NormalRule

ruleStart :: RuleName -> [(RuleName, RuleName)] -> NamedCNFRule
ruleStart name prods = NonTerminalRule name prods StartRule

compileGrammar :: NamedCNFGrammar -> CNFGrammar
compileGrammar grammar = map compileRule grammar
  where
    idxMap :: M.Map RuleName RuleNumber
    idxMap = M.fromList (zip (map ruleName grammar) [1..])

    lookupName :: RuleName -> RuleNumber
    lookupName k = fromJust $ M.lookup k idxMap

    compileRule :: NamedCNFRule -> NumberedCNFRule
    compileRule (TerminalRule name symbol) =
      TerminalRule (lookupName name) symbol
    compileRule (NonTerminalRule name prods isStart) =
      NonTerminalRule (lookupName name)
      [(lookupName a, lookupName b) | (a,b) <- prods] isStart

-- Helper functions.

ruleName :: NamedCNFRule -> RuleName
ruleName (TerminalRule name _)      = name
ruleName (NonTerminalRule name _ _) = name

ruleNumber :: NumberedCNFRule -> RuleNumber
ruleNumber (TerminalRule num _)      = num
ruleNumber (NonTerminalRule num _ _) = num

isTerminalRule :: CNFRule a -> Bool
isTerminalRule (TerminalRule _ _) = True
isTerminalRule _                  = False

isNonTerminalRule :: CNFRule a -> Bool
isNonTerminalRule (NonTerminalRule _ _ _) = True
isNonTerminalRule _                       = False

isStartRule :: CNFRule a -> Bool
isStartRule (NonTerminalRule _ _ StartRule) = True
isStartRule _                               = False

terminalRuleProduces :: CNFRule a -> Symbol -> Bool
terminalRuleProduces (TerminalRule _ s) s' = (s == s')
terminalRuleProduces _                  _  = error "Terminal rule expected!"

nonTerminalRuleProductions :: NumberedCNFRule -> [(Int, Int)]
nonTerminalRuleProductions (NonTerminalRule _ prods _) = prods
nonTerminalRuleProductions  _ = error "Non-terminal rule expected!"

-- The algorithm itself
cykAlgorithm :: CNFGrammar -> [Symbol] -> Bool
cykAlgorithm grammar input = or [arr ! (1,n,x) | x <- startIndices]
  where
    n = length input
    r = length grammar
    startIndices = map ruleNumber . filter isStartRule $ grammar

    arr = runSTUArray $ do
      marr <- newArray ((1,1,1),(n,n,r)) False

      forM_ (zip [1..] input) $ \(i, ci) ->
        forM_ (filter isTerminalRule grammar) $ \rule -> do
          let j = ruleNumber rule
          when (terminalRuleProduces rule ci) $
            writeArray marr (i,1,j) True

      forM_ [2..n] $ \i ->
        forM_ [1..(n-i+1)] $ \j ->
          forM_ [1..(i-1)] $ \k ->
            forM_ (filter isNonTerminalRule grammar) $ \rule -> do
              let a = ruleNumber rule
              forM_ (nonTerminalRuleProductions rule) $ \(b,c) -> do
                e0 <- readArray marr (j,k,b)
                e1 <- readArray marr (j+k,i-k,c)
                when (e0 && e1) $
                  writeArray marr (j,i,a) True
      return marr

-- Example input.

-- S -> SS | LH | LR
-- H -> SR
-- L -> '('
-- R -> ')'
exampleGrammar :: CNFGrammar
exampleGrammar = compileGrammar
                 [ ruleStart "S" [ ("S","S"), ("L","H"), ("L","R")]
                 , ruleNonTerminal "H" [("S","R")]
                 , ruleTerminal "L" '('
                 , ruleTerminal "R" ')'
                 ]

exampleValidInput :: [Symbol]
exampleValidInput = "((((()))))"

exampleInvalidInput :: [Symbol]
exampleInvalidInput = "(()"

-- Program entry point.
main :: IO ()
main = do let validResult = cykAlgorithm exampleGrammar exampleValidInput
          putStrLn $ "Result for the valid input: " ++ (show validResult)
          let invalidResult = cykAlgorithm exampleGrammar exampleInvalidInput
          putStrLn $ "Result for the invalid input: " ++ (show invalidResult)

-- QuickCheck test.
prop_parentheses :: Gen Bool
prop_parentheses = do m <- choose (1,5)
                      input <- replicateM m $ do
                        n <- choose (1,10)
                        return $! replicate n '(' ++ replicate n ')'
                      return $! cykAlgorithm exampleGrammar (concat input)

test :: IO ()
test = quickCheck prop_parentheses
