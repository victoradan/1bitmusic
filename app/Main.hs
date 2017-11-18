module Main where

import Comp (Comp( Ph, (:|:) ), 
             comp2ticks,
             (%))
import Compiler 

-- minimal composition example...
composition = foldl1 (:|:) ( map Ph [[i] | i <- [95..100]]) 
compTicks = (comp2ticks composition) % 30000

main :: IO ()
main = wavify compTicks 1000 "test.wav"
