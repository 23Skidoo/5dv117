module CFG.Examples (balancedParentheses)
       where

import CFG.Types

-- | Example grammar: balanced parenthese.
balancedParentheses :: CNFGrammar
balancedParentheses =
  -- S -> SS | LH | LR
  -- H -> SR
  -- L -> '('
  -- R -> ')'
  compileGrammar
  [ ruleStart "S" [ ("S","S"), ("L","H"), ("L","R")]
  , ruleNonTerminal "H" [("S","R")]
  , ruleTerminal "L" '('
  , ruleTerminal "R" ')'
  ]
