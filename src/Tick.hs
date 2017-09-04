module Tick
  ( 
    Phasor,
    Tick(..), 
    Boolean,
    i, o,
    simplex,
    sI,
    run,
    num2ticks
  ) where

import Boolean
import Phasor

-- # Ticks, Phasors...
data Tick = I | O deriving (Show, Eq)

i = I
o = O

instance Boolean Tick where
  (&) I I = I
  (&) _ _ = O

  (\&) O O = O
  (\&) _ _ = I

  (#) x y | x == y  = O
          | otherwise = I

  (!) I = O
  (!) O = I


simplex :: Boolean a => a -> Int -> [a]
simplex a n = a : (replicate (n-1) ((!)a))

sI = simplex I

num2ticks :: Int -> [Tick]
num2ticks = simplex I 

