module CFG.Examples (balancedParentheses, sillyGrammar)
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

sillyGrammar :: CNFGrammar
sillyGrammar =
  compileGrammar
  [ ruleStart "S" [("S1", "S2")]
  , ruleTerminal "S1" 'O'
  , ruleTerminal "S2" 'O' ]
