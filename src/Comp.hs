module Comp
    ( 
      Comp(..),
      collapse,
    ) where

import Tick 
import Boolean

data Comp =
      Sq [IOI]
    | Ph [IOI] Steps
    | Comp :+: Comp 
    | Comp :|: Comp
    | Comp :&: Comp 
    | Comp :#: Comp 
    deriving (Show)

cFold :: ([IOI] -> b) -> ([IOI] -> Steps -> b) -> 
         (b->b->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> 
         Comp -> b
cFold f g (+:) (|:) (&:) (#:) m =
    case m of
       Sq p -> f p
       Ph p n -> g p n
       m1 :+: m2 -> rec m1 +: rec m2
       m1 :|: m2 -> rec m1 |: rec m2
       m1 :&: m2 -> rec m1 &: rec m2
       m1 :#: m2 -> rec m1 #: rec m2
    where rec = cFold f g (+:) (|:) (&:) (#:)

-- run a Phasor for n Ticks
run :: Int -> [Tick] -> [Tick]
run n xs = take n (cycle xs)

-- The following is incredibly inefficient... 
--run n xs = [(cycle xs) !! i | i <- [0..n-1]]
collapsePhasor :: [IOI] -> Steps -> [Tick]
collapsePhasor iois steps = run steps (iois >>= ioi2ticks)

-- collaps Comp into flat sequence of Phasors:bn
collapse :: Comp -> [Tick]
collapse comp = cFold (>>= ioi2ticks) collapsePhasor (++) (\&) (&) (#) comp
