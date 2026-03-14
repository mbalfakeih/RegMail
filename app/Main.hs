module Main where
    
import Data.Char
import Data.Map

data CharRange = CharRange { start :: Int, end :: Int } -- end excl

singletonRange :: Char -> CharRange
singletonRange c = CharRange { start = ord c, end = ord c + 1 } 

data Production = NonTerminal (String, [String]) | Terminal (String, CharRange)

data CFG = CFG { prods :: Map String Production, startRule :: String }

data Regex = One CharRange | Optional Regex | Star Regex | Plus Regex | Union Regex Regex | Concat Regex Regex

main :: IO ()
main = putStrLn "Hello, Haskell!"
