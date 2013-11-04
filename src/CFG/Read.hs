module CFG.Read (readCFGrammar
                ,readCNFGrammar
                )
       where

import           Control.Applicative    hiding (many, (<|>))
import           Control.Monad          (unless)
import           Data.Bifunctor         (first)
import           Data.Function          (on)
import           Data.List              (delete, find, groupBy, intercalate,
                                         sortBy)
import           Data.Maybe             (catMaybes, isJust)
import           Data.Ord               (comparing)
import qualified Data.Set               as S
import           Text.Parsec

import           CFG.Types

--------------------------------------------------------------------------------
-- CFG parsing.

-- | Parse a comma-delimited string representing a context-free grammar
-- in. Uppercase letters followed by zero or more digits act as nonterminals and
-- lowercase letters are terminals. The initial nonterminal is always called
-- S. Empty productions are allowed.
--
-- Examples:
--
-- readCFGrammar "S ->, S -> aAb, A -> foo, A -> AA"
-- readCFGrammar "S -> AA, A -> (A), A ->"
--
readCFGrammar :: String -> Either String NamedCFGrammar
readCFGrammar input = do
  rules <- first show $ parse cfgRulesP "<stdin>" input
  validateCFGrammar rules

-- | A list of CFG rule names separated by commas.
cfgRulesP :: Parsec String () [NamedCFGRule]
cfgRulesP = sepBy cfgRuleP (spaces >> char ',' >> spaces)

-- | A single CFG rule.
cfgRuleP :: Parsec String () NamedCFGRule
cfgRuleP = CFGRule <$> ruleNameP
                   <*  (spaces >> string "->" >> spaces)
                   <*> fmap pure (many ruleNameOrTerminalP)
  where
    ruleNameOrTerminalP :: Parsec String () (Either Symbol RuleName)
    ruleNameOrTerminalP = (Left <$> terminalP) <|> (Right <$> ruleNameP)

-- | A rule name: an upper-case letter followed by zero or more digits.
ruleNameP :: Parsec String () RuleName
ruleNameP = (:) <$> upper <*> (many digit)

-- | A terminal: a digit, lowercase letter or a special character.
terminalP :: Parsec String () Symbol
terminalP = charToSymbol <$>
            (lower <|> digit <|> satisfy (flip elem "()+-."))

--------------------------------------------------------------------------------
-- CFG validation.

-- | Group productions together by name.
mergeRules :: (Rule r) => [r RuleName] -> [r RuleName]
mergeRules rs = foldr mergeProductions [] grouped
  where
    sorted  = sortBy (comparing ruleName) rs
    grouped = groupBy ((==) `on` ruleName) sorted

    mergeProductions :: Rule r => [r RuleName] -> [r RuleName] -> [r RuleName]
    mergeProductions [] rest     = rest
    mergeProductions [rule] rest = rule:rest
    mergeProductions rules  rest =
      let name  = ruleName . head $ rules
          prods = concatMap ruleProductions rules
      in  (mkRule name prods) : rest

-- | Given a bunch of rules, perform various checks and produce a CFGrammar.
validateCFGrammar :: [NamedCFGRule] -> Either String NamedCFGrammar
validateCFGrammar rules = do

  -- Group productions together.
  let merged = mergeRules rules

  -- Check that the start rule exists.
  unless (isJust $ find ((==) "S" . ruleName) merged) $
    (Left "No start rule found!")

  -- Check that all mentioned rules actually exist.
  let topLevelRuleNames  = S.fromList . map ruleName $ merged
      allRuleNames       = S.fromList . concatMap extractRuleNames $ merged
      extractRuleNames r = let prods = ruleProductions r
                           in concatMap (catMaybes . map maybeRuleName) prods
      maybeRuleName      = either (const Nothing) Just
      invalidRules       = S.toList $
                           allRuleNames `S.difference` topLevelRuleNames

  unless (allRuleNames `S.isSubsetOf` topLevelRuleNames) $
    (Left $ "Unknown rule names mentioned in productions: "
     ++ intercalate "," invalidRules)

  return $ CFGrammar merged "S"

--------------------------------------------------------------------------------
-- CNF validation.

-- | Parse a comma-delimited string representing a context-free grammar in
-- CNF. Implemented on top of readCFGrammar.
readCNFGrammar :: String -> Either String NamedCNFGrammar
readCNFGrammar input = do
  cfg <- readCFGrammar input
  validateCNFGrammar cfg

-- | Given a general CFG, check that it's in CNF.
validateCNFGrammar :: NamedCFGrammar -> Either String NamedCNFGrammar
validateCNFGrammar g = do
  -- Check that only "S" can produce [].
  let rules      = allRules g
      emptyProds = map ruleName . filter (elem [] . ruleProductions) $ rules
      nonStartEmptyProds = delete "S" emptyProds

  unless (null nonStartEmptyProds) $
    (Left $ "Grammar not in CNF: non-start rules produce empty: "
     ++ (intercalate "," nonStartEmptyProds))

  -- Check that all rules are of form 'A -> BC' (where neither B or C is S) or
  -- 'A -> a'.
  let nonconforming = map ruleName . filter (not . isCNFRule) $ rules

      isCNFRule (CFGRule n prods) = all isCNFProduction prods
        where
          isCNFProduction []                   = n == "S"
          isCNFProduction [Left _]             = True
          isCNFProduction [Right r0, Right r1] = r0 /= "S" && r1 /= "S"
          isCNFProduction _                    = False

  unless (null nonconforming) $
    (Left $ "Grammar not in CNF: invalid rules found: "
     ++ (intercalate "," nonconforming))

  -- Return the result.
  return $ CNFGrammar (map toCNFRule rules) "S" (emptyProds == ["S"])

    where
      toCNFRule :: NamedCFGRule -> NamedCNFRule
      toCNFRule (CFGRule n prods) = CNFRule n prods'
        where
          prods' | n == "S"  = map toCNFProd (delete [] prods)
                 | otherwise = map toCNFProd prods

          toCNFProd [Left s]             = Left s
          toCNFProd [Right r1, Right r2] = Right (r1,r2)
          toCNFProd _                    = error "Grammar not in CNF form!"
