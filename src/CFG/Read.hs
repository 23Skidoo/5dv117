module CFG.Read
       where

import CFG.Types

-- | Parse a comma-delimited string representing a context-free grammar in
-- CNF. Uppercase letters followed by zero or more digits act as nonterminals
-- and lowercase letters are terminals. The initial nonterminal is always S.
readCNFGrammar :: String -> Either String NamedCNFGrammar
readCNFGrammar _input = undefined
