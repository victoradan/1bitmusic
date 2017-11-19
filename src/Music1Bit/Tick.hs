module Music1Bit.Tick
  ( 
    Tick(..), 
    i, o,
    simplex,
    IOI,
    Steps,
    ioi2ticks
  ) where

import Music1Bit.Boolean

type IOI = Int -- Inter-Onset-Interval
type Steps = Int  -- The number of Ticks in a Sequence (TicksCount): length([Tick])

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


ioi2ticks :: IOI -> [Tick]
ioi2ticks = simplex I 

