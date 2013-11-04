module CFG.Examples (balancedParentheses, sillyGrammar, sillyGrammar2)
       where

import Data.Either.Combinators (fromRight')

import CFG.Read
import CFG.Types

-- | Example grammar: balanced parentheses.
balancedParentheses :: CompiledCNFGrammar
balancedParentheses = compileGrammar . fromRight' . readCNFGrammar $
                      "S ->, S -> LR, S -> S1S1, S -> LH,"
                      ++ "S1 -> LR, S1 -> S1S1, S1 -> LH,"
                      ++ "H -> S1R, L -> (, R -> )"

sillyGrammar :: CompiledCNFGrammar
sillyGrammar = compileGrammar . fromRight' . readCNFGrammar $
               "S -> S1S2, S1 -> o, S2 -> o"

sillyGrammar2 :: CompiledCNFGrammar
sillyGrammar2 = compileGrammar . fromRight' . readCNFGrammar $
                "S -> S0S0, S -> S1S2, S -> S2S1,"
                ++ "S0 -> S1S2, S0 -> S2S1,"
                ++ "S1 -> l, S2 -> o"
