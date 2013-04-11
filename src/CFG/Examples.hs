module CFG.Examples (balancedParentheses, sillyGrammar, sillyGrammar2)
       where

import CFG.Read
import CFG.Types

fromRight :: Either String a -> a
fromRight = either error id

-- | Example grammar: balanced parentheses.
balancedParentheses :: CompiledCNFGrammar
balancedParentheses = compileGrammar . fromRight . readCNFGrammar $
                      "S -> SS, S -> LH, S -> LR, H -> SR, L -> (, R -> )"

sillyGrammar :: CompiledCNFGrammar
sillyGrammar = compileGrammar . fromRight . readCNFGrammar $
               "S -> S1S2, S1 -> o, S2 -> o"

sillyGrammar2 :: CompiledCNFGrammar
sillyGrammar2 = compileGrammar . fromRight . readCNFGrammar $
                "S -> SS, S -> S1S2, S -> S2S1, S1 -> 1, S2 -> 0"
