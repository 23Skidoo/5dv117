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
  ,Rule(..), Grammar(..)
  ,isStartRule, compileGrammar
  ,charToSymbol, stringToSymbols, symbolsToString
  )
  where

import           Data.Bifunctor (bimap, second)
import           Data.Char  (isAlpha, isPunctuation, toLower)
import qualified Data.Map   as M
import           Data.Maybe (fromJust)

-- Basic types.

newtype Symbol  = SymChar Char
                  deriving (Show, Eq, Ord)
type Symbols    = [Symbol]
type RuleName   = String
type RuleNumber = Int

-- General CFG.
data CFGRule ruleName = CFGRule !ruleName ![CFGProduction ruleName]
                      deriving (Eq, Show)

-- General CFG production. An empty string is represented as @[]@.
type CFGProduction ruleName = [Either Symbol ruleName]

type NamedCFGRule    = CFGRule RuleName
type NumberedCFGRule = CFGRule RuleNumber

data CFGrammar a = CFGrammar { cfgRules     :: ![CFGRule a]
                             , cfgStartRule :: !a
                             } deriving Show

type NamedCFGrammar    = CFGrammar RuleName
type CompiledCFGrammar = CFGrammar RuleNumber

-- A context-free grammar in CNF form.
data CNFRule ruleName = CNFRule !ruleName ![CNFProduction ruleName]
               deriving (Eq, Show)
-- A CFG production in CNF.
type CNFProduction ruleName = Either Symbol (ruleName, ruleName)

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
  type RuleProduction r a :: *

  ruleName   :: r a -> a
  ruleName   = ruleNumber

  ruleNumber :: r a -> a
  ruleNumber = ruleName

  ruleProductions :: r a -> [RuleProduction r a]
  mkRule          :: a -> [RuleProduction r a] -> r a
  mapRuleName     :: (a -> b) -> r a -> r b

instance Rule CFGRule where
  type RuleProduction CFGRule a = CFGProduction a

  ruleName (CFGRule name _)         = name
  ruleProductions (CFGRule _ prods) = prods
  mkRule name prods                 = CFGRule name prods

  mapRuleName f (CFGRule nam prods) =
    CFGRule (f nam) (map (map (second f)) prods)

instance Rule CNFRule where
  type RuleProduction CNFRule a = CNFProduction a

  ruleName (CNFRule name _)         = name
  ruleProductions (CNFRule _ prods) = prods
  mkRule name prods                 = CNFRule name prods

  mapRuleName f (CNFRule nam prods) =
    CNFRule (f nam) (map (second (bimap f f)) prods)

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
isStartRule :: (Eq a, Grammar g, Rule (GrammarRule g)) =>
               g a -> GrammarRule g a -> Bool
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


-- -- Helpers for working with the 'Symbol' type.

charToSymbol :: Char -> Symbol
charToSymbol c
  | isAlpha c || isPunctuation c = SymChar (toLower c)
  | otherwise = error $ "charToSymbol: '" ++ c : "' is not a valid symbol!"

stringToSymbols :: String -> Symbols
stringToSymbols s = map SymChar s

symbolsToString :: Symbols -> String
symbolsToString syms = map (\(SymChar c) -> c) syms
