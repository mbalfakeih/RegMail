module Main where
    
import Language
import Grammar

main :: IO ()
main =  putStrLn (show $ checkConsistent productions)
