module Main where
    
import Language
import Grammar
import Finitor (elaborateBudgets)
import Data.Maybe
import CfgToRegex
import Data.Map.Lazy (assocs)


getAllProds :: CFG -> [Production]
getAllProds (CFG{prods = prods}) = concatMap snd $ assocs prods

-- main :: IO ()
-- main = putStrLn $ show $ checkConsistent productions
main = do
    let cfg' = fromJust $ elaborateBudgets cfg
    putStrLn ("Grammar length: " ++ (show $ length $ getAllProds cfg'))
    print cfg'
    let regex = cfgToRegex cfg'
    print regex
