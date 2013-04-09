module CFG.Read (readCFGrammar, readCNFGrammar)
       where

import qualified CFG.Helpers.CFG     as CFG
import qualified CFG.Helpers.CNF     as CNF
import           CFG.Types

import           Control.Applicative hiding (many, (<|>))
import           Control.Monad       (foldM, unless)
import           Data.Bifunctor      (first)
import           Data.Either         (partitionEithers)
import           Data.List           (find, groupBy, partition, sortBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromJust, isJust)
import           Data.Ord            (comparing)
import qualified Data.Set            as S
import           Data.Tuple          (swap)
import           Text.Parsec

type SymbolMap = M.Map Symbol RuleName
type SymOrName = Either Symbol RuleName

-- | Parse a comma-delimited string representing a context-free grammar
-- in. Uppercase letters followed by zero or more digits act as nonterminals and
-- lowercase letters are terminals. The initial nonterminal is always called
-- S. Empty productions are allowed.
readCFGrammar :: String -> Either String NamedCFGrammar
readCFGrammar input = do
  rules <- first show $ parse cfgRulesP "<stdin>" input
  validateCFGrammar rules

-- | A list of CFG rule names separated by commas.
cfgRulesP :: Parsec String () [(RuleName, [SymOrName])]
cfgRulesP = sepBy cfgRuleP (char ',' >> spaces)

-- | A single CFG rule. Sum type representation makes it easier to extract
-- non-terminals (see 'processRules').
cfgRuleP :: Parsec String () (RuleName, [SymOrName])
cfgRuleP = do
  name <- ruleNameP
  _    <- spaces >> string "->" >> spaces
  rhs <- many ruleNameOrTerminalP
  return $ (name, rhs)
  where
    ruleNameOrTerminalP :: Parsec String () (SymOrName)
    ruleNameOrTerminalP = (Left <$> terminalP) <|> (Right <$> ruleNameP)

-- | Given a bunch of rules, perform various checks and produce a CFGrammar.
validateCFGrammar :: [(RuleName, [SymOrName])] ->
                     Either String NamedCFGrammar
validateCFGrammar g = do
  -- Replace all Lefts with Rights in non-terminal productions. We don't want to
  -- mix symbols and rule names in non-terminal rules.
  let (terms, nonterms) = partition isTerm g
      allNames          = S.fromList . map fst $ g
      allSyms           = map snd namedSyms ++ concatMap extractSyms nonterms
      namedSyms         = map (\(nam, [Left sym]) -> (nam, sym)) terms

      allSymsMap        = CFG.runNameMonad allNames $
                          foldM bindSym M.empty allSyms

      termRules         = map toTermRule namedSyms
                          ++ map (toTermRule . swap) (M.toList allSymsMap)
      nontermRules      = map (toNonTermRule allSymsMap) nonterms
      allRules          = termRules ++ nontermRules

  -- Merge all non-terminal productions with the same name. TODO: remove
  -- duplication.
      sorted            = sortBy (comparing CFG.ruleName) allRules
      grouped           = groupBy
                          (\r0 r1 ->
                            CFG.isNonTerminalRule r0
                            && CFG.isNonTerminalRule r1
                            && CFG.ruleName r0 == CFG.ruleName r1) sorted
      merged            = foldr mergeProductions [] grouped

  -- Check that the grammar contains the start rule.
  unless (S.member "S" allNames) $ (Left "No start rule found!")

  return (CFGrammar merged "S")
  where
    isTerm :: (RuleName, [SymOrName]) -> Bool
    isTerm (_,[Left _]) = True
    isTerm _            = False

    extractSyms :: (RuleName, [SymOrName]) -> [Symbol]
    extractSyms (_, l) = fst . partitionEithers $ l

    bindSym :: SymbolMap -> Symbol -> CFG.NameMonad SymbolMap
    bindSym m sym      = do n <- CFG.freshName
                            return $ M.insert sym n m

    toTermRule :: (RuleName, Symbol) -> NamedCFGRule
    toTermRule (nam, sym)             = CFGTerminalRule nam sym

    toNonTermRule :: SymbolMap -> (RuleName, [SymOrName]) -> NamedCFGRule
    toNonTermRule symMap (nam, prods) = CFGNonTerminalRule nam
                                        [(map toName prods)]
      where
        toName :: SymOrName -> RuleName
        toName (Left sym) = fromJust . M.lookup sym $ symMap
        toName (Right n)  = n

    mergeProductions :: [NamedCFGRule] -> [NamedCFGRule] -> [NamedCFGRule]
    mergeProductions [] rest     = rest
    mergeProductions [rule] rest = rule:rest
    mergeProductions rules  rest =
      let name  = CFG.ruleName . head $ rules
          prods = concatMap CFG.nonTerminalRuleProductions rules
      in  (CFGNonTerminalRule name prods) : rest


-- | Parse a comma-delimited string representing a context-free grammar in
-- CNF. Uppercase letters followed by zero or more digits act as nonterminals
-- and lowercase letters are terminals. The initial nonterminal is always called
-- S.
readCNFGrammar :: String -> Either String NamedCNFGrammar
readCNFGrammar input = do rules <- first show $ parse cnfRulesP "<stdin>" input
                          validateCNFGrammar rules

-- | A list of CNF rule names separated by commas.
cnfRulesP :: Parsec String () [NamedCNFRule]
cnfRulesP = sepBy cnfRuleP (char ',' >> spaces)

-- | A rule name, followed by '<-', followed either by a terminal (lowercase
-- letter, digit or a punctuation symbol) or by two names of non-terminals.
cnfRuleP :: Parsec String () NamedCNFRule
cnfRuleP = do
  name  <- ruleNameP
  _     <- spaces >> string "->" >> spaces
  mTerm <- optionMaybe terminalP
  case mTerm of
    Just t  -> return $ CNFTerminalRule name t
    Nothing -> do
      rhs <- pure (,) <*> ruleNameP <*> ruleNameP
      return $ CNFNonTerminalRule name [rhs]

-- | A rule name: an upper-case letter followed by zero or more digits.
ruleNameP :: Parsec String () String
ruleNameP = (:) <$> upper <*> (many digit)

-- | A terminal: a digit, lowercase letter or a special character.
terminalP :: Parsec String () Symbol
terminalP = charToSymbol <$>
            (lower <|> digit <|> satisfy (flip elem "()+-."))

-- | Given a bunch of CNF rules, perform various checks on them.
validateCNFGrammar :: [NamedCNFRule] -> Either String NamedCNFGrammar
validateCNFGrammar g = do
  -- Group productions of non-terminals together (not actually necessary).
  let sorted  = sortBy (comparing CNF.ruleName) g
      grouped = groupBy (\r0 r1 -> CNF.isNonTerminalRule r0
                                   && CNF.isNonTerminalRule r1
                                   && CNF.ruleName r0 == CNF.ruleName r1) sorted
      merged  = foldr mergeProductions [] grouped

  -- Check that the start rule exists.
  unless (isJust $ find (\r -> CNF.ruleName r == "S") merged) $
    (Left "No start rule found!")

  -- TODO: Add more validity checks. Check whether the grammar produces an empty
  -- string.
  -- Check that terminal rules are unique and don't intersect with nonterminals.
  return (CNFGrammar merged "S" False)

  where
    mergeProductions :: [NamedCNFRule] -> [NamedCNFRule] -> [NamedCNFRule]
    mergeProductions [] rest     = rest
    mergeProductions [rule] rest = rule:rest
    mergeProductions rules  rest =
      let name  = CNF.ruleName . head $ rules
          prods = concatMap CNF.nonTerminalRuleProductions rules
      in  (CNFNonTerminalRule name prods) : rest
