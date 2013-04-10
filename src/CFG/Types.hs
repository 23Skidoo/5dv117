-- | Basic types and helper functions for constructing a context-free grammar.
module CFG.Types (
  -- * Types.
   Symbol
  ,RuleName, RuleNumber

  ,CFGRule(..), CFGrammar(..)
  ,NamedCFGRule, NumberedCFGRule
  ,NamedCFGrammar, CompiledCFGrammar

  ,CNFRule(..), CNFGrammar(..)
  ,NamedCNFRule, NumberedCNFRule
  ,NamedCNFGrammar, CompiledCNFGrammar

  -- * Misc. helper functions.
  ,Rule(..)
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
data CNFRule a = CNFTerminalRule !a !Symbol
               | CNFNonTerminalRule !a ![(a, a)]
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

class Rule r where

  ruleName :: r a -> a
  ruleName = ruleNumber

  ruleNumber :: r a -> a
  ruleNumber = ruleName

  isTerminalRule :: r a -> Bool
  isNonTerminalRule :: r a -> Bool

  terminalRuleProduces :: r a -> Symbol -> Bool


instance Rule CFGRule where

--  ruleName :: CFGRule a -> a
  ruleName (CFGTerminalRule name _)    = name
  ruleName (CFGNonTerminalRule name _) = name

--  isTerminalRule :: CFGRule a -> Bool
  isTerminalRule (CFGTerminalRule _ _) = True
  isTerminalRule _                     = False

--  isNonTerminalRule :: CFGRule a -> Bool
  isNonTerminalRule (CFGNonTerminalRule _ _) = True
  isNonTerminalRule _                        = False

--  terminalRuleProduces :: CFGRule a -> Symbol -> Bool
  terminalRuleProduces (CFGTerminalRule _ s) s' = (s == s')
  terminalRuleProduces _ _ = error "Terminal rule expected!"


instance Rule CNFRule where

--  ruleName :: CNFRule a -> a
  ruleName (CNFTerminalRule name _)    = name
  ruleName (CNFNonTerminalRule name _) = name

--  isTerminalRule :: CNFRule a -> Bool
  isTerminalRule (CNFTerminalRule _ _) = True
  isTerminalRule _                     = False

--  isNonTerminalRule :: CFGRule a -> Bool
  isNonTerminalRule (CNFNonTerminalRule _ _) = True
  isNonTerminalRule _                        = False

--  terminalRuleProduces :: CNFRule a -> Symbol -> Bool
  terminalRuleProduces (CNFTerminalRule _ s) s' = (s == s')
  terminalRuleProduces _ _ = error "Terminal rule expected!"

-- Helpers for working with the 'Symbol' type.
charToSymbol :: Char -> Symbol
charToSymbol = SymChar

stringToSymbols :: String -> Symbols
stringToSymbols s = map SymChar s

symbolsToString :: Symbols -> String
symbolsToString syms = map (\(SymChar c) -> c) syms
