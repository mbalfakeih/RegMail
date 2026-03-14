module Language where

import Data.Char
import Data.Map

data CharRange = CharRange { start :: Int, end :: Int } -- end excl

singletonRange :: Char -> CharRange
singletonRange c = CharRange { start = ord c, end = ord c + 1 } 

data Production = NonTerminal String String String | Terminal String CharRange

data CFG = CFG { prods :: Map String [Production], startRule :: String, budgets :: Map String Budget }

data Regex = One CharRange | Optional Regex | Star Regex | Plus Regex | Union Regex Regex | Concat Regex Regex

data Budget = Unlimited | Finite Int
