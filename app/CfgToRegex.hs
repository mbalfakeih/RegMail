module CfgToRegex where

import Language
import Control.Monad.State.Lazy
import Data.Map.Lazy
import Prelude hiding (lookup)
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.Bifunctor (second)
import Data.List (foldl1')

prodToRegex :: CFG -> Production -> State (Map String Regex) Regex 
prodToRegex _ (Terminal _ cr) = if start cr == end cr then return Epsilon else return (One cr)
prodToRegex cfg (NonTerminal _ l r) = do
    lr <- varToRegex cfg l
    rr <- varToRegex cfg r
    return (doConcat lr rr)

varToRegex :: CFG -> String -> State (Map String Regex) Regex
varToRegex cfg v = do
    let ps = fromJust $ lookup v (prods cfg)
    rs <- mapM (prodToRegex cfg) ps
    let res = foldl1' doUnion rs 
    modify (insert v res)
    return res

doConcat :: Regex -> Regex -> Regex
doConcat l Epsilon = l
doConcat Epsilon r = r
doConcat l r = Concat l r

doUnion :: Regex -> Regex -> Regex
doUnion Epsilon Epsilon = Epsilon
doUnion (Optional l) Epsilon = Optional l
doUnion l Epsilon = Optional l
doUnion Epsilon (Optional r) = Optional r
doUnion Epsilon r = Optional r
doUnion (Optional l) (Optional r) = Optional (Union l r)
doUnion l r = Union l r

cfgToRegex :: CFG -> Regex
-- cfgToRegex cfg@(CFG {startRule = start}) = let (res,s) = runState (varToRegex cfg [] start) Data.Map.Lazy.empty in trace ((show $ (second (unnestOptionals . removeEps) <$> assocs s)) ++ "\n\n") unnestOptionals $ removeEps res
cfgToRegex cfg@(CFG {startRule = start}) = let (res,s) = runState (varToRegex cfg start) Data.Map.Lazy.empty in res
