module Main where
    
import Language

addrSpec :: Production 
addrSpec = NonTerminal "addr-spec" ["local-part", "at-sign", "domain"]

atSign :: Production
atSign = Terminal "at-sign" $ singletonRange '@'

main :: IO ()
main = undefined