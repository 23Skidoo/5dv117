-- | Helpers for working with context-free grammars in Chomsky normal form.
module CFG.Helpers.CNF (
  --  * Helpers for constructing the grammar.
  compileGrammar

  -- * Misc. helper functions.
  ,ruleName, ruleNumber
  ,isTerminalRule, isNonTerminalRule, isStartRule
  ,terminalRuleProduces, nonTerminalRuleProductions
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

-- Misc. helper functions.

ruleName :: NamedCNFRule -> RuleName
ruleName (CNFTerminalRule name _)    = name
ruleName (CNFNonTerminalRule name _) = name

ruleNumber :: NumberedCNFRule -> RuleNumber
ruleNumber (CNFTerminalRule num _)    = num
ruleNumber (CNFNonTerminalRule num _) = num

isTerminalRule :: CNFRule a -> Bool
isTerminalRule (CNFTerminalRule _ _) = True
isTerminalRule _                     = False

isNonTerminalRule :: CNFRule a -> Bool
isNonTerminalRule (CNFNonTerminalRule _ _) = True
isNonTerminalRule _                        = False

isStartRule :: (Eq a) => CNFGrammar a -> CNFRule a -> Bool
isStartRule g (CNFNonTerminalRule name _) | cnfStartRule g == name = True
                                          | otherwise              = False
isStartRule _ (CNFTerminalRule _ _)                                = False

terminalRuleProduces :: CNFRule a -> Symbol -> Bool
terminalRuleProduces (CNFTerminalRule _ s) s' = (s == s')
terminalRuleProduces _                  _  = error "Terminal rule expected!"

nonTerminalRuleProductions :: CNFRule a -> [(a, a)]
nonTerminalRuleProductions (CNFNonTerminalRule _ prods) = prods
nonTerminalRuleProductions  _ = error "Non-terminal rule expected!"
