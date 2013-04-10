module CFG.Read (readCFGrammar, readCNFGrammar)
       where

import qualified CFG.Helpers.CFG     as CFG
import           CFG.Types

import           Control.Applicative hiding (many, (<|>))
import           Control.Monad       (foldM, unless)
import           Data.Bifunctor      (first)
import           Data.Either         (partitionEithers)
import           Data.List           (find, groupBy, intercalate
                                     ,partition, sortBy)
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

  -- Group productions of all nonterminals together.
  let merged            = mergeRules allRules

  validateCommon merged
  return (CFGrammar merged "S")

  where
    isTerm :: (RuleName, [SymOrName]) -> Bool
    isTerm (_,[Left _]) = True
    isTerm _            = False

    extractSyms :: (RuleName, [SymOrName]) -> [Symbol]
    extractSyms (_, l) = fst . partitionEithers $ l

    bindSym :: SymbolMap -> Symbol -> CFG.NameMonad SymbolMap
    bindSym m sym = do n <- CFG.freshName
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
      rhs <- pure Pair <*> ruleNameP <*> ruleNameP
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
  -- Group productions of nonterminals together.
  let merged = mergeRules g

  -- A parsed grammar in CNF never produces an empty string, this can happen
  -- only when converting a general CFG to CNF.
  let producesEmpty = False

  validateCommon merged
  return (CNFGrammar merged "S" producesEmpty)

-- | Group productions of nonterminals together by name.
mergeRules :: (Rule r) => [r RuleName] -> [r RuleName]
mergeRules rs = foldr mergeProductions [] grouped
  where
    sorted  = sortBy (comparing ruleName) rs
    grouped = groupBy (\r0 r1 -> isNonTerminalRule r0
                                 && isNonTerminalRule r1
                                 && ruleName r0 == ruleName r1) sorted

    mergeProductions :: Rule r => [r RuleName] -> [r RuleName] -> [r RuleName]
    mergeProductions [] rest     = rest
    mergeProductions [rule] rest = rule:rest
    mergeProductions rules  rest =
      let name  = ruleName . head $ rules
          prods = concatMap nonTerminalRuleProductions rules
      in  (mkNonTerminal name prods) : rest

-- | Common checks.
validateCommon :: Rule r => [r RuleName] -> Either String ()
validateCommon rules = do
    -- Check that the start rule exists.
  unless (isJust $ find (\r -> ruleName r == "S") rules) $
    (Left "No start rule found!")

  -- Check that terminal rule names are unique.
  let termNames    = map ruleName . filter isTerminalRule $ rules
      termNamesSet = S.fromList termNames
      nonUnique    = intercalate ", " . map head
                     . filter ((>1) . length) . groupBy (==) $ termNames
  unless (S.size termNamesSet == length termNames) $
    (Left $ "Some terminal rule names are not unique: " ++ nonUnique)

  -- Check that terminal rule names don't intersect with nonterminals.
  let nontermNames = map ruleName . filter isNonTerminalRule $ rules
      nontermNamesSet = S.fromList nontermNames
      intersection = intercalate ", " . S.toList $
                     nontermNamesSet `S.intersection` termNamesSet

  unless (null intersection) $
    (Left $ "Some non-terminal rule names clash with non-terminals: "
     ++ intersection)
