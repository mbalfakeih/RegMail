module CfgToRegex where

import Language
import Control.Monad.State.Lazy
import Data.Map.Lazy
import Prelude hiding (lookup)
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.Bifunctor (second)

prodToRegex :: CFG -> [String] -> Production -> State (Map String Regex) Regex 
prodToRegex _ _ (Terminal _ cr) = if start cr == end cr then return Epsilon else return (One cr)
prodToRegex cfg stack (NonTerminal n l r) = do
    if n `elem` stack then return Epsilon else do
        lr <- varToRegex cfg (stack) l
        rr <- varToRegex cfg (stack) r
        return (Concat lr rr)

varToRegex :: CFG -> [String] -> String -> State (Map String Regex) Regex
varToRegex cfg stack v = do
    let ps = fromJust $ lookup v (prods cfg)
    rs <- mapM (prodToRegex cfg stack) ps
    let res = Prelude.foldl1 Union rs 
    modify (insert v res)
    return res

removeEps :: Regex -> Regex
removeEps (Union a b) = case (removeEps a, removeEps b) of
    (Epsilon, Epsilon) -> Epsilon
    (a, Epsilon) -> Optional a
    (Epsilon, b) -> Optional b
    (a, b) -> Union a b
removeEps (Concat a b) = case (removeEps a, removeEps b) of
    (Epsilon, Epsilon) -> Epsilon
    (a, Epsilon) -> a
    (Epsilon, b) -> b
    (a, b) -> Concat a b
removeEps x = x

unnestOptionals :: Regex -> Regex
-- unnestOptionals x = x
unnestOptionals (Union a b) = Union (unnestOptionals a) (unnestOptionals b)
unnestOptionals (Concat a b) = Concat (unnestOptionals a) (unnestOptionals b)
unnestOptionals (Optional a) = case unnestOptionals a of
    (Optional a') -> a'
    x -> Optional x
unnestOptionals x = x

cfgToRegex :: CFG -> Regex
-- cfgToRegex cfg@(CFG {startRule = start}) = let (res,s) = runState (varToRegex cfg [] start) Data.Map.Lazy.empty in trace ((show $ (second (unnestOptionals . removeEps) <$> assocs s)) ++ "\n\n") unnestOptionals $ removeEps res
cfgToRegex cfg@(CFG {startRule = start}) = let (res,s) = runState (varToRegex cfg [] start) Data.Map.Lazy.empty in unnestOptionals $ removeEps res
