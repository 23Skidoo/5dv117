module CFG.Examples (balancedParentheses, sillyGrammar, sillyGrammar2)
       where

import CFG.Helpers.CNF
import CFG.Types

-- | Example grammar: balanced parenthese.
balancedParentheses :: CompiledCNFGrammar
balancedParentheses =
  -- S -> SS | LH | LR
  -- H -> SR
  -- L -> '('
  -- R -> ')'
  listToGrammar
  [ ruleNonTerminal "S" [ ("S","S"), ("L","H"), ("L","R")]
  , ruleNonTerminal "H" [("S","R")]
  , ruleTerminal "L" '('
  , ruleTerminal "R" ')'
  ]

sillyGrammar :: CompiledCNFGrammar
sillyGrammar =
  listToGrammar
  [ ruleNonTerminal "S" [("S1", "S2")]
  , ruleTerminal "S1" 'o'
  , ruleTerminal "S2" 'o' ]

sillyGrammar2 :: CompiledCNFGrammar
sillyGrammar2 =
  listToGrammar
  [ ruleNonTerminal "S" [("S", "S"), ("S1", "S1")]
  , ruleTerminal "S1" '1'
  , ruleTerminal "S1" '0' ]
