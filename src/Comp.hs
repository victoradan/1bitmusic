module Comp
    ( 
      Comp(..),
      eval,
      (%),
      comp2ticks
    ) where

import Tick 
import Boolean

data Comp a =
      Sq a
    | Ph a
    | Comp a :+: Comp a
    | Comp a :|: Comp a
    | Comp a :&: Comp a
    | Comp a :#: Comp a
    deriving (Show)

cFold :: (a -> b) -> (a -> b) -> 
        (b->b->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> 
        Comp a -> b
cFold f g (+:) (|:) (&:) (#:) m =
    case m of
       Sq p -> f p
       Ph p -> g p
       m1 :+: m2 -> rec m1 +: rec m2
       m1 :|: m2 -> rec m1 |: rec m2
       m1 :&: m2 -> rec m1 &: rec m2
       m1 :#: m2 -> rec m1 #: rec m2
    where rec = cFold f g (+:) (|:) (&:) (#:)

-- TODO : implement this via `cFold`
{-cMap :: (a -> b) -> (a -> b) -> Comp a -> b-}
{-cMap f g comp = cFold f g (:+:) (:|:) (:&:) (:#:) comp-}
cMap :: (a -> b) -> (a -> b) -> Comp a -> Comp b
cMap f g (Sq a) =  Sq (f a)
cMap f g (Ph a) =  Ph (g a)
cMap f g (c1 :+: c2) = cMap f g c1 :+: cMap f g c2
cMap f g (c1 :|: c2) = cMap f g c1 :|: cMap f g c2
cMap f g (c1 :&: c2) = cMap f g c1 :&: cMap f g c2
cMap f g (c1 :#: c2) = cMap f g c1 :#: cMap f g c2

(%) :: Comp (Phasor a) -> Int -> Comp (Phasor a)
(%) comp n = cMap id (run n) comp
{-(%) comp n = cMap (Sq . id) (Ph . (run n)) comp-}

eval :: Boolean a => Comp (Phasor a) -> (Phasor a)
{-eval comp = cFold (>>=  num2ticks) (>>=  num2ticks) (++) (\&) (&) (#) comp-}
eval comp = cFold id id (++) (\&) (&) (#) comp

comp2ticks :: Comp [Int] -> Comp [Tick]
comp2ticks = cMap (>>= num2ticks) (>>= num2ticks) 

