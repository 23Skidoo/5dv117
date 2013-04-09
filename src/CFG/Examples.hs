module CFG.Examples (balancedParentheses, sillyGrammar, sillyGrammar2)
       where

import           CFG.Types

import qualified CFG.Helpers.CNF as CNF
import           CFG.Read

fromRight :: Either String a -> a
fromRight = either error id

-- | Example grammar: balanced parentheses.
balancedParentheses :: CompiledCNFGrammar
balancedParentheses = CNF.compileGrammar . fromRight . readCNFGrammar $
                      "S -> SS, S -> LH, S -> LR, H -> SR, L -> (, R -> )"

sillyGrammar :: CompiledCNFGrammar
sillyGrammar = CNF.compileGrammar . fromRight . readCNFGrammar $
               "S -> S1S2, S1 -> o, S2 -> o"

sillyGrammar2 :: CompiledCNFGrammar
sillyGrammar2 = CNF.compileGrammar . fromRight . readCNFGrammar $
                "S -> SS, S -> S1S1, S1 -> 1, S1 -> 0"
