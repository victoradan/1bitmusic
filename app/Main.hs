module Main where

import Comp (Comp( Ph, (:|:) )) 
import Compiler 

-- minimal composition example...
composition = foldl1 (:|:) [Ph [i] steps | i <- [95..100], let steps = 30000]

main :: IO ()
main = wavify composition 1000 "test.wav"
