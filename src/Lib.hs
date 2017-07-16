module Lib
    ( 
      Seq,
      Tick, 
      i, o,
      run,
      rot,
      (&), (\&), (#),
			roll,
			equalize,
      someFunc
    ) where


someFunc  = do
  print $ (run [i,o,o,i] 6) # (run [i,i,o] 9)

data Tick = I | O deriving (Show, Eq)

i = I
o = O

type Seq = [Tick]
-- Should we replace Seq with Phasor?
-- Should Phasor be a Type or a Type class?
{-type Phasor = [Tick]-}

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
  (&)  xs ys = zipWith (&) xs ys
  (\&) xs ys = zipWith (\&) xs ys
  (#)  xs ys = zipWith (#) xs ys
  {-(&) (x:xs) (y:ys) = (x & y) :  (xs & ys)-}
  {-(&) [] ys = ys-}
  {-(&) xs [] = xs-}


-- Phasor Functions

-- rotate a Phasor
rot :: (Integral n) => Seq -> n -> Seq
rot xs n 
  | n >  0 = rot (last xs : init xs) (n-1) 
  | n <  0 = rot (tail xs ++ [head xs]) (n+1)
  | n == 0 = xs 

-- run a Phasor for n Ticks
run :: Seq -> Int -> Seq
run xs n = [xs !! (i `mod` length xs) | i <- [0..n-1]]


-- length xs must be > length hs
roll :: Seq -> Seq -> Seq
roll [] _ = []
roll _ [] = []
roll xs (h:hs)
	| h == I = (xs ++ zeros (length hs)) \& (O : (roll xs hs))
  | h == O = zeros (length xs + length hs) \& (O : (roll xs hs))

zeros :: Int -> Seq
zeros n = replicate n O

equalize :: Seq -> Seq -> (Seq, Seq)
equalize xs ys 
	| length xs > length ys = (xs, ys ++ zeros absDiff)
	| length xs < length ys = (xs ++ zeros absDiff, ys)
  | otherwise = (xs, ys)
		where absDiff = abs(length xs - length ys)

-- GRAPH

data Graph a b = Empty
             | Vertex a
             | Overlay (Graph a b) (Graph a b)
             | Edge (Graph a b) (Graph a b) b
             deriving (Show)
