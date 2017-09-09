module Phasor
  (
    Phasor,
    run
  ) where

type Phasor a = [a]

-- run a Phasor for n Ticks
run :: Int -> Phasor a -> Phasor a
run n xs = take n (cycle xs)
-- The following is incredibly inefficient... 
--run n xs = [(cycle xs) !! i | i <- [0..n-1]]
