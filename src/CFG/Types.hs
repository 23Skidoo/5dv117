-- | Basic types and helper functions for constructing a context-free grammar.
module CFG.Types (
  -- * Types.
   Symbol
  ,RuleName, RuleNumber
  ,IsStartRule(..)
  ,CNFRule(..)
  ,NamedCNFRule, NumberedCNFRule
  ,NamedCNFGrammar, CNFGrammar

  --  * Helpers for constructing the grammar.
  ,ruleTerminal, ruleNonTerminal, ruleStart
  ,compileGrammar

  -- * Misc. helper functions.
  ,ruleName, ruleNumber
  ,isTerminalRule, isNonTerminalRule, isStartRule
  ,terminalRuleProduces, nonTerminalRuleProductions
  ,charToSymbol, stringToSymbols, symbolsToString
  )
       where

import qualified Data.Map   as M
import           Data.Maybe (fromJust)


-- Grammar ADT definition.

newtype Symbol  = SymChar Char
                  deriving (Show, Eq)
type Symbols    = [Symbol]
type RuleName   = String
type RuleNumber = Int

data IsStartRule = StartRule | NormalRule
                 deriving (Eq,Show)

data CNFRule a = TerminalRule !a !Symbol
               | NonTerminalRule !a ![(a, a)] !IsStartRule
               deriving (Eq, Show)

type NamedCNFRule    = CNFRule RuleName
type NumberedCNFRule = CNFRule RuleNumber

type NamedCNFGrammar = [NamedCNFRule]
type CNFGrammar = [NumberedCNFRule]

-- Helpers for constructing the grammar.

ruleTerminal :: RuleName -> Char -> NamedCNFRule
ruleTerminal name prod = TerminalRule name (SymChar prod)

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

-- Misc. helper functions.

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

nonTerminalRuleProductions :: CNFRule a -> [(a, a)]
nonTerminalRuleProductions (NonTerminalRule _ prods _) = prods
nonTerminalRuleProductions  _ = error "Non-terminal rule expected!"

charToSymbol :: Char -> Symbol
charToSymbol = SymChar

stringToSymbols :: String -> Symbols
stringToSymbols s = map SymChar s

symbolsToString :: Symbols -> String
symbolsToString syms = map (\(SymChar c) -> c) syms
