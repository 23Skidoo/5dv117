{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

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
  ,Rule(..), Grammar(..), isStartRule, compileGrammar
  ,charToSymbol, stringToSymbols, symbolsToString
  )
  where

import qualified Data.Map   as M
import           Data.Maybe (fromJust)

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

data CFGrammar a = CFGrammar { cfgRules     :: ![CFGRule a]
                             , cfgStartRule :: !a
                             } deriving Show

type NamedCFGrammar    = CFGrammar RuleName
type CompiledCFGrammar = CFGrammar RuleNumber

-- A context-free grammar in CNF form.
data CNFRule a = CNFTerminalRule !a !Symbol
               | CNFNonTerminalRule !a ![(a,a)]
               deriving (Eq, Show)

type NamedCNFRule    = CNFRule RuleName
type NumberedCNFRule = CNFRule RuleNumber

data CNFGrammar a = CNFGrammar { cnfRules         :: ![CNFRule a]
                               , cnfStartRule     :: !a
                               , cnfProducesEmpty :: !Bool
                               } deriving Show

type NamedCNFGrammar    = CNFGrammar RuleName
type CompiledCNFGrammar = CNFGrammar RuleNumber

-- Basic helpers.

-- | Helpers for rules.
class Rule (r :: * -> *) where
  type NonTermProduction r a :: *

  ruleName   :: r a -> a
  ruleName   = ruleNumber
  ruleNumber :: r a -> a
  ruleNumber = ruleName

  isTerminalRule    :: r a -> Bool
  isNonTerminalRule :: r a -> Bool

  terminalRuleProduces       :: r a -> Symbol -> Bool
  nonTerminalRuleProductions :: r a -> [NonTermProduction r a]

  mkNonTerminal :: a -> [NonTermProduction r a] -> r a
  mapRuleName   :: (a -> b) -> r a -> r b

instance Rule CFGRule where
  type NonTermProduction CFGRule a = [a]

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

  mapRuleName f (CFGTerminalRule nam sym)      = CFGTerminalRule (f nam) sym
  mapRuleName f (CFGNonTerminalRule nam prods) = CFGNonTerminalRule (f nam)
                                                 (map (map f) prods)

instance Rule CNFRule where
  type NonTermProduction CNFRule a = (a,a)

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

  mapRuleName f (CNFTerminalRule nam sym)      = CNFTerminalRule (f nam) sym
  mapRuleName f (CNFNonTerminalRule nam prods) = CNFNonTerminalRule (f nam)
                                                 [(f a, f b) | (a,b) <- prods]

-- | Helpers for grammars.
class Grammar g where
  type GrammarRule g :: * -> *

  startRule    :: g a -> a
  allRules     :: g a -> [GrammarRule g a]
  mapRuleNames :: (a -> b) -> g a -> g b

instance Grammar CFGrammar where
  type GrammarRule CFGrammar = CFGRule

  startRule g = cfgStartRule g
  allRules    = cfgRules

  mapRuleNames f (CFGrammar rules start) =
    CFGrammar (map (mapRuleName f) rules) (f start)

instance Grammar CNFGrammar where
  type GrammarRule CNFGrammar = CNFRule

  startRule g = cnfStartRule g
  allRules    = cnfRules

  mapRuleNames f (CNFGrammar rules start e) =
    CNFGrammar (map (mapRuleName f) rules) (f start) e

-- | Is this a start rule for this grammar?
isStartRule :: (Eq a, Grammar g, Rule r) => g a -> r a -> Bool
isStartRule g r | (startRule g == ruleName r) = True
                | otherwise                   = False

-- | Convert a grammar to a representation that uses numbers instead of strings
-- for rule names.
compileGrammar :: (Grammar g, Rule (GrammarRule g)) => g RuleName -> g RuleNumber
compileGrammar g = mapRuleNames lookupName g
  where
    rules = allRules g

    idxMap :: M.Map RuleName RuleNumber
    idxMap = M.fromList (zip (map ruleName rules) [1..])

    lookupName :: RuleName -> RuleNumber
    lookupName k = fromJust $ M.lookup k idxMap


-- Helpers for working with the 'Symbol' type.

charToSymbol :: Char -> Symbol
charToSymbol = SymChar

stringToSymbols :: String -> Symbols
stringToSymbols s = map SymChar s

symbolsToString :: Symbols -> String
symbolsToString syms = map (\(SymChar c) -> c) syms
