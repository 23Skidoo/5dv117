-- | Helpers for working with context-free grammars in Chomsky normal form.
module CFG.Helpers.CNF (
  --  * Helpers for constructing the grammar.
  compileGrammar
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
      [((lookupName a), (lookupName b)) | (a, b) <- prods]
