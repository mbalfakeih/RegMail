{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language where

import Data.Char
import Data.Map
import Data.Maybe (fromMaybe)

data CharRange = CharRange { start :: Int, end :: Int } -- end excl

singletonRange :: Char -> CharRange
singletonRange c = CharRange { start = ord c, end = ord c + 1 } 

data Production = NonTerminal String String String | Terminal String CharRange

data CFG = CFG { prods :: Map String [Production], startRule :: String, budgets :: Map String Budget }

modifyBudgets :: Map String Budget -> CFG -> CFG
modifyBudgets b (CFG { .. }) = CFG { prods, startRule, budgets = b }

modifyProds :: Map String [Production] -> CFG -> CFG
modifyProds ps (CFG {..}) = CFG {prods = ps, startRule, budgets}

data Regex = One CharRange | Optional Regex | Star Regex | Plus Regex | Union Regex Regex | Concat Regex Regex

data Budget = Unlimited | Finite Int

showWithBudget :: Budget -> Production -> String
showWithBudget Unlimited (NonTerminal n l r) = n ++ " -> " ++ l ++ " " ++ r
showWithBudget Unlimited (Terminal n (CharRange {..})) = n ++ " -> " ++ "{" ++ show start ++ "-" ++ show end ++ "}"
showWithBudget (Finite b) (NonTerminal n l r) = n ++ "(<= " ++ show b ++ ") -> " ++ l ++ " " ++ r
showWithBudget (Finite b) (Terminal n (CharRange {..})) = n ++ "(<= " ++ show b ++ ") -> " ++ "{" ++ show start ++ "-" ++ show end ++ "}"

instance Show CFG where
    show (CFG {..}) = "Start: " ++ startRule ++ "\n" ++ unlines (showOneVar <$> assocs prods)
        where
            showOneVar (v, ps) = v ++ ": \n" ++ unlines (showWithBudget (fromMaybe Unlimited $ Data.Map.lookup v budgets) <$> ps)