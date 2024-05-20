module Music1Bit.Music where
import Data.Bits 

import Music1Bit.Types

data Primitive = Imp IOI | Phasor [IOI] Dur deriving (Show)

data Music
  = Prim Primitive
  | Music :+: Music -- sequential
  | Music :=: Music -- parallel
  deriving (Show)

imp :: IOI -> Music
imp i = Prim (Imp i)

phasor :: Int -> [IOI] -> Music
phasor n iois = Prim $ Phasor iois n

sequential :: [Music] -> Music
sequential [] = Prim (Imp 0)
sequential (i : is) = i :+: sequential is

parallel :: [Music] -> Music
parallel [] = Prim (Imp 0)
parallel (i : is) = i :=: parallel is

foldMusic prim seq par m =
  case m of
    Prim p -> prim p
    m1 :+: m2 -> seq (rec m1) (rec m2)
    m1 :=: m2 -> par (rec m1) (rec m2)
  where
    rec = foldMusic prim seq par

-- run a Phasor for n Ticks
run :: Int -> [Tick] -> [Tick]
run n xs = take n (cycle xs)

collapsePhasor :: [IOI] -> Int -> [Tick]
collapsePhasor iois steps = run steps (iois >>= ioi2ticks)

ioi2ticks :: IOI -> [Tick]
ioi2ticks 0 = []
ioi2ticks ioi = True : replicate (ioi - 1) False

collapse :: Music -> [Tick]
collapse (Prim (Imp ioi)) = ioi2ticks ioi
collapse (Prim (Phasor iois dur)) = collapsePhasor iois dur -- take dur $ cycle $ concatMap ioi2ticks iois
collapse (m1 :+: m2) = concat [collapse m1, collapse m2]-- collapse m1 ++ collapse m2
collapse (m1 :=: m2) =
  let m1' = collapse m1
      m2' = collapse m2
      l = length m1' - length m2'
      (m1'', m2'') = if l > 0 then (m1', concat [m2', replicate (abs l) False]) else (concat [m1', replicate (abs l) False], m2')
   in zipWith (/=) m1'' m2''

intToBin :: Integer -> [Bool]
intToBin 0 = []
intToBin n = reverse (helper n)
  where
    helper 0 = []
    helper n | even n = False : helper (n `div` 2)
    helper n  = True : helper (n `div` 2)

-- do integerLog2 instead: https://hackage.haskell.org/package/arithmoi-0.4.2.0/docs/Math-NumberTheory-Logarithms.html
bitCount :: Integer -> Int
bitCount = (+1) . floor . logBase 2 . fromInteger
