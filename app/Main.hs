module Main where
    
import Data.Char
import Data.Map

data CharRange = CharRange { start :: Int, end :: Int } -- end excl

singletonRange :: Char -> CharRange
singletonRange c = CharRange { start = ord c, end = ord c + 1 } 

data Production a = NonTerminal String a [String] | Terminal String CharRange

data CFG a = CFG { prods :: Map String (Production a), startRule :: String }

data Regex = One CharRange | Optional Regex | Star Regex | Plus Regex | Union Regex Regex | Concat Regex Regex

data Budget = Unlimited | Finite Int

addr-spec :: Production Budget 
addr-spec = NonTerminal "addr-spec" Unlimited ["local-part", "at-sign", "domain"]

at-sign :: Production Finite 1
at-sign = singletonRange '@'
