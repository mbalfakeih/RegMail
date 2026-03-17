{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language where

import Data.Char
import Data.Map
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import Text.Read (Lexeme(String))

data CharRange = CharRange { start :: Int, end :: Int } deriving Show -- end excl

singletonRange :: Char -> CharRange
singletonRange c = CharRange { start = ord c, end = ord c + 1 } 

data Production = NonTerminal String String String | Terminal String CharRange deriving Show

data CFG = CFG { prods :: Map String [Production], startRule :: String, budgets :: Map String Budget }

modifyBudgets :: Map String Budget -> CFG -> CFG
modifyBudgets b (CFG { .. }) = CFG { prods, startRule, budgets = b }

modifyProds :: Map String [Production] -> CFG -> CFG
modifyProds ps (CFG {..}) = CFG {prods = ps, startRule, budgets}

data Regex = One CharRange | Optional Regex | Union Regex Regex | Concat Regex Regex | Epsilon

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

printCharRegex :: Int -> String
printCharRegex i = if isAscii c && isAlphaNum c && notElem c "\\/.*+?{}()=:^$" then c:"" else "\\x" ++ showHexByte i
    where
        c = chr i
        showHexByte :: Int -> String
        showHexByte i = if length (showHex i "") == 2 then showHex i "" else "0" ++ showHex i ""

instance Show Regex where
    show (One CharRange {..}) = if end - start == 1 then printCharRegex start else "[" ++ printCharRegex start ++ "-" ++ printCharRegex end ++ "]"
    show (Optional r) = "(" ++ show r ++ ")?"
    show (Union r1 r2) = "" ++ show r1 ++ "|" ++ show r2 ++ ""
    show (Concat r1 r2) = "(" ++ show r1 ++ ")" ++ "(" ++ show r2 ++ ")"
    show Epsilon = "(eps)"