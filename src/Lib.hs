module Lib
    ( 
      Phasor,
      Tick(..), 
      i, o,
      run,
      Comp(..),
      eval,
      (&), (\&), (#), (%),
      simplex,
      sI,
      num2ticks,
      comp2ticks
    ) where


data Comp a =
      Sq a
    | Ph a
    | Comp a :+: Comp a
    | Comp a :|: Comp a
    | Comp a :&: Comp a
    | Comp a :#: Comp a
    deriving (Show)

fold :: (a -> b) -> (a -> b) -> 
        (b->b->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> 
        Comp a -> b
fold f g (+:) (|:) (&:) (#:) m =
    case m of
       Sq p -> f p
       Ph p -> g p
       m1 :+: m2 -> rec m1 +: rec m2
       m1 :|: m2 -> rec m1 |: rec m2
       m1 :&: m2 -> rec m1 &: rec m2
       m1 :#: m2 -> rec m1 #: rec m2
    where rec = fold f g (+:) (|:) (&:) (#:)

-- TODO : implement this via `fold`
{-cMap :: (a -> b) -> (a -> b) -> Comp a -> b-}
{-cMap f g comp = fold f g (:+:) (:|:) (:&:) (:#:) comp-}
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
{-eval comp = fold (>>=  num2ticks) (>>=  num2ticks) (++) (\&) (&) (#) comp-}
eval comp = fold id id (++) (\&) (&) (#) comp

comp2ticks :: Comp [Int] -> Comp [Tick]
comp2ticks = cMap (>>= num2ticks) (>>= num2ticks) 

num2ticks :: Int -> [Tick]
num2ticks = simplex I 

-- # Ticks, Phasors...
data Tick = I | O deriving (Show, Eq)

i = I
o = O

class Boolean a where
  (&)  :: a -> a -> a
  (\&) :: a -> a -> a
  (#)  :: a -> a -> a
  (!) :: a -> a

instance Boolean Tick where
  (&) I I = I
  (&) _ _ = O

  (\&) O O = O
  (\&) _ _ = I

  (#) x y | x == y  = O
          | otherwise = I

  (!) I = O
  (!) O = I

instance Boolean Char where
  (&) 'i' 'i' = 'i' 
  (&)  _   _  = 'o'

  (\&) 'o' 'o' = 'o'
  (\&)  _   _  = 'i'

  (#) x y | x == y  = 'o'
          | otherwise = 'i'

  (!) 'i' = 'o'
  (!)  _  = 'i'

instance  Boolean a => Boolean [a] where
  (&)  xs ys = zipWith (&)  xs ys
  (\&) xs ys = zipWith (\&) xs ys
  (#)  xs ys = zipWith (#)  xs ys
  (!) (x:xs) = (!)x : map (!) xs

type Phasor a = [a]

-- # Phasor Functions
-- run a Phasor for n Ticks
run :: Int -> Phasor a -> Phasor a
run n xs = [(cycle xs) !! i | i <- [0..n-1]]
--run xs n = [xs !! (i `mod` length xs) | i <- [0..n-1]]


-- constructors
simplex :: Boolean a => a -> Int -> [a]
simplex a n = a : (replicate (n-1) ((!)a))

sI = simplex I
