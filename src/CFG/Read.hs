module CFG.Read (readCFGrammar, readCNFGrammar)
       where

import CFG.Types
import CFG.CNFHelpers

import Control.Applicative hiding (many, (<|>))
import Control.Monad       (unless)
import Data.Bifunctor      (first)
import Data.Char           (isPunctuation)
import Data.Either         ()
import Data.List           (find, groupBy, sortBy)
import Data.Maybe          (isJust)
import Data.Ord            (comparing)
import Text.Parsec

readCFGrammar :: String -> Either String NamedCFGrammar
readCFGrammar _input = undefined

-- | Parse a comma-delimited string representing a context-free grammar in
-- CNF. Uppercase letters followed by zero or more digits act as nonterminals
-- and lowercase letters are terminals. The initial nonterminal is always called
-- S.
readCNFGrammar :: String -> Either String NamedCNFGrammar
readCNFGrammar input = do g <- first show $ parse rulesP "<stdin>" input
                          validateCNFGrammar g

-- | A list of rule names separated by commas.
rulesP :: Parsec String () [NamedCNFRule]
rulesP = sepBy ruleP (char ',' >> spaces)

-- | A rule name, followed by '<-', followed either by a terminal (lowercase
-- letter, digit or a punctuation symbol) or by two names of non-terminals.
ruleP :: Parsec String () NamedCNFRule
ruleP = do
  name  <- ruleNameP
  _     <- spaces >> string "->" >> spaces
  mTerm <- optionMaybe (charToSymbol <$>
                        (lower <|> satisfy isPunctuation <|> digit))
  case mTerm of
    Just t  -> return $ CNFTerminalRule name t
    Nothing -> do
      rhs <- pure (,) <*> ruleNameP <*> ruleNameP
      return $ CNFNonTerminalRule name [rhs]

-- | A rule name: an upper-case letter followed by zero or more digits.
ruleNameP :: Parsec String () String
ruleNameP = (:) <$> upper <*> (many digit)

validateCNFGrammar :: [NamedCNFRule] -> Either String NamedCNFGrammar
validateCNFGrammar g = do
  -- Group productions of non-terminals together (not actually necessary).
  let sorted  = sortBy (comparing ruleName) g
      grouped = groupBy (\r0 r1 -> isNonTerminalRule r0
                                   && isNonTerminalRule r1
                                   && ruleName r0 == ruleName r1) sorted
      merged  = foldr mergeProductions [] grouped

  -- Check that the start rule exists.
  unless (isJust $ find (\r -> ruleName r == "S") merged) $
    (Left "No start rule found!")

  -- TODO: Add more validity checks. Check whether the grammar produces an empty
  -- string.
  return (CNFGrammar merged "S" False)

  where
    mergeProductions :: [NamedCNFRule] -> [NamedCNFRule] -> [NamedCNFRule]
    mergeProductions [] rest     = rest
    mergeProductions [rule] rest = rule:rest
    mergeProductions rules  rest =
      let (CNFNonTerminalRule name _) = head rules
          prods = concatMap nonTerminalRuleProductions rules
      in  (CNFNonTerminalRule name prods) : rest
