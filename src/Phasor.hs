module Phasor
  (
    Phasor,
    run
  ) where

type Phasor a = [a]

-- run a Phasor for n Ticks
run :: Int -> Phasor a -> Phasor a
run n xs = [(cycle xs) !! i | i <- [0..n-1]]
--run xs n = [xs !! (i `mod` length xs) | i <- [0..n-1]]


