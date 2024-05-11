module Music1Bit.Music where

type Tick = Bool
type Dur = Int
type IOI = Int

data Primitive = Imp IOI -- | Phasor [IOI] Dur 

data Music =
      Prim Primitive 
    | Music :+: Music -- sequential
    | Music :=: Music -- parallel

imp :: IOI -> Music
imp i = Prim (Imp i)

sequential :: [Music] -> Music
sequential (i:is) = i :+: sequential is
sequential [] = Prim (Imp 0)

cFold prim seq par m =
    case m of
       Prim p -> prim p
       m1 :+: m2 -> seq (rec m1) (rec m2)
       m1 :=: m2 -> par (rec m1) (rec m2)
    where rec = cFold prim seq par

-- -- run a Phasor for n Ticks
-- run :: Int -> [Tick] -> [Tick]
-- run n xs = take n (cycle xs)

-- collapsePhasor :: [IOI] -> Int -> [Tick]
-- collapsePhasor iois steps = run steps (iois >>= ioi2ticks)


ioi2ticks :: IOI -> [Tick]
ioi2ticks  0 = []
ioi2ticks  ioi = True : replicate (ioi-1) False

collapse :: Music -> [Tick]
collapse (Prim (Imp ioi)) = ioi2ticks ioi
collapse (m1 :+: m2) = collapse m1 ++ collapse m2
collapse (m1 :=: m2) = zipWith (/=) (collapse m1) (collapse m2)