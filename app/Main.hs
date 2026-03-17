module Main where
    
import Language
import Grammar
import Finitor (elaborateBudgets)
import Data.Maybe
import CfgToRegex
import Data.Map.Lazy (assocs)
import System.IO (hPutStrLn, stderr, hPrint)


getAllProds :: CFG -> [Production]
getAllProds (CFG{prods = prods}) = concatMap snd $ assocs prods

main :: IO ()
main = do
    let cfg' = fromJust $ elaborateBudgets cfg
    -- hPutStrLn stderr ("Grammar length: " ++ (show $ length $ getAllProds cfg'))
    -- hPrint stderr cfg'
    let regex = cfgToRegex cfg'
    print regex
