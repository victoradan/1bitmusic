module Comp
    ( 
      Comp(..),
      collapse,
      {-(%),-}
      {-comp2ticks-}
    ) where

import Tick 
import Boolean

type IOI = Int
type Steps = Int

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

-- TODO : implement this via `cFold`
{-cMap :: (a -> b) -> (a -> b) -> Comp a -> b-}
{-cMap f g comp = cFold f g (:+:) (:|:) (:&:) (:#:) comp-}

{-cMap :: (a -> b) -> (a -> c -> b) -> Comp -> Comp -}
{-cMap f g (Sq a) =  Sq (f a)-}
{-cMap f g (Ph a n) =  Ph (g a n)-}
{-cMap f g (c1 :+: c2) = cMap f g c1 :+: cMap f g c2-}
{-cMap f g (c1 :|: c2) = cMap f g c1 :|: cMap f g c2-}
{-cMap f g (c1 :&: c2) = cMap f g c1 :&: cMap f g c2-}
{-cMap f g (c1 :#: c2) = cMap f g c1 :#: cMap f g c2-}

{-(%) :: Comp -> Int -> [Tick]-}
{-(%) comp n = cMap id (run n) comp-}

collapsePhasor :: [IOI] -> Steps -> [Tick]
collapsePhasor iois steps = run steps (iois >>= num2ticks)

-- collaps Comp into flat sequence of Phasors:bn
collapse :: Comp -> (Phasor Tick)
collapse comp = cFold (>>= num2ticks) collapsePhasor (++) (\&) (&) (#) comp
{-collapse comp = cFold (>>=  num2ticks) (>>=  num2ticks) (++) (\&) (&) (#) comp-}

-- comp2ticks :: Comp [Int] -> Comp [Tick]
-- comp2ticks = cMap (>>= num2ticks) (>>= num2ticks) 

