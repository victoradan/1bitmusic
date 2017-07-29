module Lib
    ( 
      Phasor,
      Tick, 
      i, o,
      run,
      rot,
      (&), (\&), (#),
			conv,
			equalize,
      someFunc
    ) where


someFunc  = do
  print $ (run [i,o,o,i] 6) # (run [i,i,o] 9)

data Tick = I | O deriving (Show, Eq)

i = I
o = O

type Phasor = [Tick]
-- Should Phasor be a Type or a Type class?
-- Should it be a newtype? 
--
type Seq = [Phasor]


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


-- Phasor Functions

-- rotate a Phasor
rot :: (Integral n) => Phasor -> n -> Phasor
rot xs n 
  | n >  0 = rot (last xs : init xs) (n-1) 
  | n <  0 = rot (tail xs ++ [head xs]) (n+1)
  | n == 0 = xs 


-- run a Phasor for n Ticks
run :: Phasor -> Int -> Phasor
--run xs n = [xs !! (i `mod` length xs) | i <- [0..n-1]]
run xs n = [(cycle xs) !! i | i <- [0..n-1]]

-- You cannot define these two functions
(%) :: Phasor -> Int -> Phasor
(%) seq n = run seq n

-- TODO: replace fixed-length seqs with infinite `cycle `
-- length xs must be > length hs
conv :: Phasor -> Phasor -> Phasor
conv [] _ = []
conv _ [] = []
conv xs (h:hs)
  | h == I = (xs ++ zeros (length hs)) \& (O : (conv xs hs))
  | h == O = zeros (length xs + length hs) \& (O : (conv xs hs))

-- TODO: remove: this is the same as : 
-- [o] % n
zeros :: Int -> Phasor
zeros n = replicate n O

-- get rid of?
equalize :: Phasor -> Phasor -> (Phasor, Phasor)
equalize xs ys 
	| length xs > length ys = (xs, ys ++ zeros absDiff)
	| length xs < length ys = (xs ++ zeros absDiff, ys)
  | otherwise = (xs, ys)
		where absDiff = abs(length xs - length ys)

