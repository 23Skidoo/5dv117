module CFG.Examples (balancedParentheses, sillyGrammar, sillyGrammar2)
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
  , ruleTerminal "S1" 'o'
  , ruleTerminal "S2" 'o' ]

sillyGrammar2 :: CNFGrammar
sillyGrammar2 =
  compileGrammar
  [ ruleStart "S" [("S", "S"), ("S1", "S1")]
  , ruleTerminal "S1" '1'
  , ruleTerminal "S1" '0' ]
