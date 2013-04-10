-- | Helpers for working with context-free grammars in Chomsky normal form.
module CFG.Helpers.CNF (
  --  * Helpers for constructing the grammar.
  compileGrammar

  -- * Misc. helper functions.
  ,isStartRule, nonTerminalRuleProductions
  )
  where

import           CFG.Types

import qualified Data.Map   as M
import           Data.Maybe (fromJust)

compileGrammar :: NamedCNFGrammar -> CompiledCNFGrammar
compileGrammar (CNFGrammar rules start e) =
  CNFGrammar (map compileRule rules) (lookupName start) e
  where
    idxMap :: M.Map RuleName RuleNumber
    idxMap = M.fromList (zip (map ruleName rules) [1..])

    lookupName :: RuleName -> RuleNumber
    lookupName k = fromJust $ M.lookup k idxMap

    compileRule :: NamedCNFRule -> NumberedCNFRule
    compileRule (CNFTerminalRule name symbol) =
      CNFTerminalRule (lookupName name) symbol
    compileRule (CNFNonTerminalRule name prods) =
      CNFNonTerminalRule (lookupName name)
      [(lookupName a, lookupName b) | (a,b) <- prods]

isStartRule :: (Eq a) => CNFGrammar a -> CNFRule a -> Bool
isStartRule g r | (cnfStartRule g == ruleName r) = True
                | otherwise                      = False

nonTerminalRuleProductions :: CNFRule a -> [(a, a)]
nonTerminalRuleProductions (CNFNonTerminalRule _ prods) = prods
nonTerminalRuleProductions  _ = error "Non-terminal rule expected!"
