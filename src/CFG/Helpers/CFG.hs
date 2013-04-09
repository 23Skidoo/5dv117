-- | Helpers for working with general context-free grammars.
module CFG.Helpers.CFG (
    -- * Basic helpers.
    ruleName, ruleNumber, isNonTerminalRule, nonTerminalRuleProductions
    -- * Monad for fresh names.
   ,NameMonad, runNameMonad, freshName, rememberName
  )
  where

import CFG.Types

import           Control.Monad.State
import qualified Data.Set            as S

-- Basic helpers
ruleName :: NamedCFGRule -> RuleName
ruleName (CFGTerminalRule name _)    = name
ruleName (CFGNonTerminalRule name _) = name

ruleNumber :: NumberedCFGRule -> RuleNumber
ruleNumber (CFGTerminalRule num _)    = num
ruleNumber (CFGNonTerminalRule num _) = num

isNonTerminalRule :: CFGRule a -> Bool
isNonTerminalRule (CFGNonTerminalRule _ _) = True
isNonTerminalRule _                        = False

nonTerminalRuleProductions :: CFGRule a -> [[a]]
nonTerminalRuleProductions (CFGNonTerminalRule _ prods) = prods
nonTerminalRuleProductions  _ = error "Non-terminal rule expected!"


-- Monad for generating fresh names.
data NameState = NameState { nameStateCounter :: !Int
                           , nameStateSet     :: !(S.Set RuleName) }
                 deriving Show
type NameMonad = State NameState

-- | Initial state, for using in conjunction with 'runState'.
runNameMonad :: S.Set RuleName -> NameMonad a -> a
runNameMonad s act = fst $ runState act (NameState 0 s)

-- | Generate a fresh name.
freshName :: NameMonad RuleName
freshName = do
  c <- getCounter
  incCounter
  let n = "Z" ++ (show c)
  hasSeen <- nameSetContains n
  if hasSeen
    then freshName
    else do rememberName n
            return n
  where
    getCounter :: NameMonad Int
    getCounter = gets nameStateCounter

    incCounter :: NameMonad  ()
    incCounter = modify
                 (\s -> s { nameStateCounter = (+1) . nameStateCounter $ s })

    nameSetContains :: RuleName -> NameMonad Bool
    nameSetContains n = S.member n `fmap` gets nameStateSet

-- | Remember a given name. A remembered name will never be produced by
-- 'freshName'.
rememberName :: RuleName -> NameMonad ()
rememberName n = modify
                 (\s -> s { nameStateSet = S.insert n . nameStateSet $ s})
