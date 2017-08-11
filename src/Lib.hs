module Lib
    ( 
      Phasor,
      Tick, 
      i, o,
      run,
      {-rot,-}
      (&), (\&), (#),
			{-conv,-}
			{-equalize,-}
      (%)
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
eval comp = fold id id (++) (\&) (&) (#) comp

-- # Ticks, Phasors...
data Tick = I | O deriving (Show, Eq)

i = I
o = O

class Boolean a where
  (&)  :: a -> a -> a
  (\&) :: a -> a -> a
  (#)  :: a -> a -> a

instance Boolean Tick where
  (&) I I = I
  (&) _ _ = O

  (\&) O O = O
  (\&) _ _ = I

  (#) x y | x == y  = O
          | otherwise = I

instance  Boolean a => Boolean [a] where
  (&)  xs ys = zipWith (&)  xs ys
  (\&) xs ys = zipWith (\&) xs ys
  (#)  xs ys = zipWith (#)  xs ys
  {-(&) (x:xs) (y:ys) = (x & y) :  (xs & ys)-}
  {-(&) [] ys = ys-}
  {-(&) xs [] = xs-}

type Phasor a = [a]

-- # Phasor Functions
-- run a Phasor for n Ticks
run :: Int -> Phasor a -> Phasor a
run n xs = [(cycle xs) !! i | i <- [0..n-1]]
--run xs n = [xs !! (i `mod` length xs) | i <- [0..n-1]]

{-(%) :: Phasor a -> Int -> Phasor a-}
{-(%) seq n = run seq n-}


-- rotate a Phasor
{-rot :: (Integral n) => Phasor -> n -> Phasor-}
{-rot xs n -}
  {-| n >  0 = rot (last xs : init xs) (n-1) -}
  {-| n <  0 = rot (tail xs ++ [head xs]) (n+1)-}
  {-| n == 0 = xs -}

-- TODO: replace fixed-length seqs with infinite `cycle `
-- length xs must be > length hs
{-conv :: Phasor -> Phasor -> Phasor-}
{-conv [] _ = []-}
{-conv _ [] = []-}
{-conv xs (h:hs)-}
  {-| h == I = (xs ++ zeros (length hs)) \& (O : (conv xs hs))-}
  {-| h == O = zeros (length xs + length hs) \& (O : (conv xs hs))-}


-- get rid of?
{-equalize :: Phasor -> Phasor -> (Phasor, Phasor)-}
{-equalize xs ys -}
	{-| length xs > length ys = (xs, ys ++ zeros absDiff)-}
	{-| length xs < length ys = (xs ++ zeros absDiff, ys)-}
  {-| otherwise = (xs, ys)-}
		{-where absDiff = abs(length xs - length ys)-}

