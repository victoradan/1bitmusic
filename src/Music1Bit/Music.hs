module Music1Bit.Music where

import           Pipes
import qualified Pipes.Prelude as Pipes

type Tick = Bool
type Dur = Int
type IOI = Int

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
sequential []       = Prim (Imp 0)
sequential (i : is) = i :+: sequential is

parallel :: [Music] -> Music
parallel []       = Prim (Imp 0)
parallel (i : is) = i :=: parallel is

foldMusic prim seq par m =
  case m of
    Prim p    -> prim p
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
ioi2ticks 0   = [False]
ioi2ticks ioi = True : replicate (ioi - 1) False

collapse :: Music -> [Tick]
collapse m = Pipes.toList $ go m
  where
    go (Prim (Imp ioi)) = do
      each (ioi2ticks ioi)
    go (Prim (Phasor iois dur)) = do
      each $ collapsePhasor iois dur
    go (m1 :+: m2) = do
      -- each $ Pipes.toList $ go m1
      -- each $ Pipes.toList $ go m2
      for (go m1) yield
      for (go m2) yield
    go (m1 :=: m2) =
      let m1' = Pipes.toList $ go m1
          m2' = Pipes.toList $ go m2
          l = max (length m1') (length m2')
          m1'' = cycle m1'
          m2'' = cycle m2'
       in -- (m1'', m2'') = if l > 0 then (m1', concat [m2', replicate (abs l) False]) else (concat [m1', replicate (abs l) False], m2')
          do
            each $ take l $ zipWith (/=) m1'' m2''

intToBin :: Integer -> [Bool]
intToBin 0 = []
intToBin n = reverse (helper n)
  where
    helper 0 = []
    helper n | even n = False : helper (n `div` 2)
    helper n = True : helper (n `div` 2)

-- do integerLog2 instead: https://hackage.haskell.org/package/arithmoi-0.4.2.0/docs/Math-NumberTheory-Logarithms.html
bitCount :: Integer -> Int
bitCount = (+ 1) . floor . logBase 2 . fromInteger
