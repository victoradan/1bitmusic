module Main where

import Lib
import Audio

tick2ints seq = [if s == I then 1 else 0 | s <- seq]

test :: IO ()
test = writeMono "test.wav" $ seq2audio 20 $ tick2ints (eval (s % 10000))
  where s = Ph [i,o,o,i,o,o,i,o,o,o,o,o,o,o] :|: 
            Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:
            Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:
            Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:
            Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o] 

main :: IO ()
main = test
