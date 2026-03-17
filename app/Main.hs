module Main where
    
import Language
import Grammar
import Finitor (elaborateBudgets)
import Data.Maybe
import CfgToRegex
import Data.Map.Lazy (assocs)
import System.IO (hPutStrLn, stderr, hPrint)
import System.Environment
import Control.Monad



main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ error "Specify budget for local and domain part"
    let cfg' = fromJust $ elaborateBudgets $ cfg (read $ head args) (read $ args !! 1)
    -- hPutStrLn stderr ("Grammar length: " ++ (show $ length $ getAllProds cfg'))
    -- hPrint stderr cfg'
    let regex = cfgToRegex cfg'
    print regex
