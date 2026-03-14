module Main where
    
import Language

addrSpec :: Production Budget 
addrSpec = NonTerminal "addr-spec" Unlimited ["local-part", "at-sign", "domain"]

atSign :: Production a
atSign = Terminal "at-sign" $ singletonRange '@'

main :: IO ()
main = undefined