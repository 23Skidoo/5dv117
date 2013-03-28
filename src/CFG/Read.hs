module CFG.Read (readCNFGrammar)
       where

import CFG.Types

import Control.Applicative hiding (many, (<|>))
import Control.Monad       (when)
import Data.Bifunctor      (first)
import Data.Char           (isPunctuation)
import Data.Either         ()
import Data.List           (groupBy, sortBy)
import Data.Ord            (comparing)
import Text.Parsec

-- | Parse a comma-delimited string representing a context-free grammar in
-- CNF. Uppercase letters followed by zero or more digits act as nonterminals
-- and lowercase letters are terminals. The initial nonterminal is always called
-- S.
readCNFGrammar :: String -> Either String NamedCNFGrammar
readCNFGrammar input = do g <- first show $ parse rulesP "<stdin>" input
                          validateGrammar g

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
    Just t  -> return $ TerminalRule name t
    Nothing -> do
      rhs <- pure (,) <*> ruleNameP <*> ruleNameP
      let isStart = if name == "S" then StartRule else NormalRule
      return $ NonTerminalRule name [rhs] isStart

-- | A rule name: an upper-case letter followed by zero or more lower-case
-- letters.
ruleNameP :: Parsec String () String
ruleNameP = (:) <$> upper <*> (many digit)

validateGrammar :: NamedCNFGrammar -> Either String NamedCNFGrammar
validateGrammar g = do
  -- Group productions of non-terminals together (not actually necessary).
  let sorted  = sortBy (comparing ruleName) g
      grouped = groupBy (\r0 r1 -> isNonTerminalRule r0
                                   && isNonTerminalRule r1
                                   && ruleName r0 == ruleName r1) sorted
      merged  = foldr merge [] grouped
      numStartRules = length . filter isStartRule $ merged

  -- Check that there is only one start rule.
  when (numStartRules > 1) $ (Left "More than one start rule!")

  -- TODO: Add more validity checks.
  return merged

  where
    merge :: [NamedCNFRule] -> [NamedCNFRule] -> [NamedCNFRule]
    merge [] rest     = rest
    merge [rule] rest = rule:rest
    merge rules  rest =
      let (NonTerminalRule name _ isStart) = head rules
          prods = concatMap nonTerminalRuleProductions rules
      in  (NonTerminalRule name prods isStart) : rest
