module CFG.CNF (convertToCNF)
       where

-- import qualified Data.Set as S

import CFG.Types

-- | CFG-to-CNF conversion.
-- See M.Sipser, "Introduction to the Theory of Computation", 2nd ed, p.109.
convertToCNF :: NamedCFGrammar -> NamedCNFGrammar
convertToCNF = undefined
-- convertToCNF (CFGrammar rules _) = convertedToCNF
--   where
--     names :: [RuleName]
--     names = map ruleName rules

--     nameSet :: S.Set RuleName
--     nameSet = S.fromList names

--     -- Introduce a new start rule.
--     withNewStartRule :: NamedCFGrammar
--     withNewStartRule =
--       runNameMonad nameSet $ do
--         n <- freshName
--         let newStart = CFGNonTerminalRule n [["S"]]
--         return (CFGrammar (newStart:rules) n)

--     -- start :: RuleName
--     -- start = cfgStartRule withNewStartRule

--     -- Take care of all 0 rules.
--     withoutEmptyRules :: NamedCFGrammar
--     withoutEmptyRules = undefined withNewStartRule

--     -- Take care of unit rules.
--     withoutUnitRules :: NamedCFGrammar
--     withoutUnitRules = undefined withoutEmptyRules

--     -- Convert to proper form.
--     convertedToCNF :: NamedCFGrammar
--     convertedToCNF = undefined withoutUnitRules



-- removeEmptySublists :: [[a]] -> [[a]]
-- removeEmptySublists = foldl' (\l e -> case e of [] -> l; _ -> e:l) []


-- removeEmpty :: [CFGRule a] -> Maybe (a, CFGRule a)
-- removeEmpty []     = Nothing
-- removeEmpty (r:rs) = case doRemove r of
--   Nothing -> removeEmpty rs
--   Just r  -> Just r

-- doRemove r@(CFGTerminalRule _ _)           = Nothing
-- doRemove r@(CFGNonTerminalRule name prods) =
--   Just (CFGNonTerminalRule name (removeEmptySublists prods))
