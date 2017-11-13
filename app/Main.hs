module Main where

import Audio
import Comp
import Tick

seq2ints onVal seq = [if s == onVal then 1 else 0 | s <- seq]
tick2ints = seq2ints I

wavify composition rate fileName = 
  writeMono fileName $ seq2audio rate $ tick2ints $ eval composition

composition = foldl1 (:|:) ( map Ph [[i] | i <- [95..100]]) 
compTicks = (comp2ticks composition) % 30000

main :: IO ()
main = wavify compTicks 1000 "test.wav"
