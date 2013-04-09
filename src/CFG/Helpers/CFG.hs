-- | Helpers for working with general context-free grammars.
module CFG.Helpers.CFG (
    --  * Helpers for constructing the grammar.
   ruleTerminal, ruleNonTerminal
  )
  where

import CFG.Types

ruleTerminal :: RuleName -> Char -> NamedCFGRule
ruleTerminal name prod = CFGTerminalRule name (charToSymbol prod)

ruleNonTerminal :: RuleName -> [[RuleName]] -> NamedCFGRule
ruleNonTerminal name prods = CFGNonTerminalRule name prods
