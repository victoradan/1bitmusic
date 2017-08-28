module Main where

import Lib
import Audio

seq2ints on seq = [if s == on then 1 else 0 | s <- seq]
tick2ints = seq2ints I
char2ints = seq2ints 'i'

test :: IO ()
{-test = writeMono "test.wav" $ seq2audio 20 $ tick2ints (eval (ss % 10000))-}
  {-where ss = Ph [i,o,o,i,o,o,i,o,o,o,o,o,o,o] :|: -}
             {-Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:-}
             {-Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:-}
             {-Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:-}
             {-Ph [i,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o] :|:-}
{-test = writeMono "test.wav" $ seq2audio 20 $ char2ints (eval (ss % 10000))-}
  {-where ss = Ph "iooooooooooooooooooooo" :|: Ph "ioooooooooioooooooio"-}
test = writeMono "test.wav" $ seq2audio 1000 $ tick2ints (eval (ss % 10000))
  where ss = Ph (I: replicate 100 O) :|: Ph (I: replicate 99 O)
  {-where ss = foldl (\comp seq -> comp :|: seq) (Ph [O]) [Ph (sI 100), Ph (sI 99), Ph (sI 98)]-}
  {-where ss = Ph (sI 100) :|: Ph (sI 99) :|: Ph (sI 98)-}
-- add support for integers??
{-test = writeMono "test.wav" $ seq2audio 20 $ char2ints (eval (ss % 10000))-}
  {-where ss = Ph 512 :|: Ph 7-}

main :: IO ()
main = test
