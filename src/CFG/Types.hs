{-# LANGUAGE TypeFamilies #-}
-- | Basic types and helper functions for constructing a context-free grammar.
module CFG.Types (
  -- * Types.
   Symbol
  ,RuleName, RuleNumber

  ,CFGRule(..), CFGrammar(..)
  ,NamedCFGRule, NumberedCFGRule
  ,NamedCFGrammar, CompiledCFGrammar

  ,Pair(..), CNFRule(..), CNFGrammar(..)
  ,NamedCNFRule, NumberedCNFRule
  ,NamedCNFGrammar, CompiledCNFGrammar

  -- * Misc. helper functions.
  ,Rule(..), Grammar(..), isStartRule
  ,charToSymbol, stringToSymbols, symbolsToString
  )
  where

-- Basic types.

newtype Symbol  = SymChar Char
                  deriving (Show, Eq, Ord)
type Symbols    = [Symbol]
type RuleName   = String
type RuleNumber = Int

-- General CFG. A rule that generates the empty string is represented as
-- @CFGNonTerminalRule "RuleName" [[]]@
data CFGRule a = CFGTerminalRule !a !Symbol
               | CFGNonTerminalRule !a ![[a]]
               deriving (Eq, Show)

type NamedCFGRule    = CFGRule RuleName
type NumberedCFGRule = CFGRule RuleNumber

data CFGrammar a = CFGrammar { cfgRules  :: [CFGRule a]
                             , cfgStartRule :: a
                             } deriving Show

type NamedCFGrammar    = CFGrammar RuleName
type CompiledCFGrammar = CFGrammar RuleNumber

-- A context-free grammar in CNF form.
data Pair a = Pair !a !a
            deriving (Eq, Show)
data CNFRule a = CNFTerminalRule !a !Symbol
               | CNFNonTerminalRule !a ![Pair a]
               deriving (Eq, Show)

type NamedCNFRule    = CNFRule RuleName
type NumberedCNFRule = CNFRule RuleNumber

data CNFGrammar a = CNFGrammar { cnfRules         :: [CNFRule a]
                               , cnfStartRule     :: a
                               , cnfProducesEmpty :: Bool
                               } deriving Show

type NamedCNFGrammar    = CNFGrammar RuleName
type CompiledCNFGrammar = CNFGrammar RuleNumber

-- Basic helpers.

-- | Helpers for rules.
class Rule r where
  type NonTermProduction r :: * -> *

  ruleName   :: r a -> a
  ruleName   = ruleNumber
  ruleNumber :: r a -> a
  ruleNumber = ruleName

  isTerminalRule    :: r a -> Bool
  isNonTerminalRule :: r a -> Bool

  terminalRuleProduces       :: r a -> Symbol -> Bool
  nonTerminalRuleProductions :: r a -> [NonTermProduction r a]

  mkNonTerminal :: a -> [NonTermProduction r a] -> r a

instance Rule CFGRule where
  type NonTermProduction CFGRule = []

  ruleName (CFGTerminalRule name _)    = name
  ruleName (CFGNonTerminalRule name _) = name

  isTerminalRule (CFGTerminalRule _ _) = True
  isTerminalRule _                     = False

  isNonTerminalRule (CFGNonTerminalRule _ _) = True
  isNonTerminalRule _                        = False

  terminalRuleProduces (CFGTerminalRule _ s) s' = (s == s')
  terminalRuleProduces _ _ = error "Terminal rule expected!"

  nonTerminalRuleProductions (CFGNonTerminalRule _ prods) = prods
  nonTerminalRuleProductions _ = error "Nonterminal rule expected!"

  mkNonTerminal name prods = CFGNonTerminalRule name prods

instance Rule CNFRule where
  type NonTermProduction CNFRule = Pair

  ruleName (CNFTerminalRule name _)    = name
  ruleName (CNFNonTerminalRule name _) = name

  isTerminalRule (CNFTerminalRule _ _) = True
  isTerminalRule _                     = False

  isNonTerminalRule (CNFNonTerminalRule _ _) = True
  isNonTerminalRule _                        = False

  terminalRuleProduces (CNFTerminalRule _ s) s' = (s == s')
  terminalRuleProduces _ _ = error "Terminal rule expected!"

  nonTerminalRuleProductions (CNFNonTerminalRule _ prods) = prods
  nonTerminalRuleProductions _ = error "Nonterminal rule expected!"

  mkNonTerminal name prods = CNFNonTerminalRule name prods


-- | Helpers for grammars.
class Grammar g where
  startRule :: g a -> a

instance Grammar CNFGrammar where
  startRule g = cnfStartRule g

instance Grammar CFGrammar where
  startRule g = cfgStartRule g

isStartRule :: (Eq a, Grammar g, Rule r) => g a -> r a -> Bool
isStartRule g r | (startRule g == ruleName r) = True
                | otherwise                   = False


-- Helpers for working with the 'Symbol' type.
charToSymbol :: Char -> Symbol
charToSymbol = SymChar

stringToSymbols :: String -> Symbols
stringToSymbols s = map SymChar s

symbolsToString :: Symbols -> String
symbolsToString syms = map (\(SymChar c) -> c) syms
