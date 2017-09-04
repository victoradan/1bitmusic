module Main where

import Lib
import Audio

seq2ints on seq = [if s == on then 1 else 0 | s <- seq]
tick2ints = seq2ints I
char2ints = seq2ints 'i'

test :: IO ()
test = writeMono "test.wav" $ seq2audio 500 $ tick2ints (eval ((comp2ticks ss ) % 30000))
  where ss = foldl1 (:|:) ( map Ph [[i] | i <- [95..100]]) 
  -- where ss = Ph [100] :|: Ph [99] :|: Ph [98] :|: Ph [97]

main :: IO ()
main = test
