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

-- Helpers for working with the 'Symbol' type.
charToSymbol :: Char -> Symbol
charToSymbol = SymChar

stringToSymbols :: String -> Symbols
stringToSymbols s = map SymChar s

symbolsToString :: Symbols -> String
symbolsToString syms = map (\(SymChar c) -> c) syms
