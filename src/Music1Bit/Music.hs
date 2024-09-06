module Music1Bit.Music where

import qualified Music1Bit.Combinators as C
import           Music1Bit.Types

data Phasor a = Phasor {phase :: Integer -> Integer, iois ::[IOI], imps:: [a]}

data Music a
  = Prim Dur (Phasor a)
  | (Music a) :+: (Music a) -- sequential
  | (Music a) :=: (Music a)  -- or
  | (Music a) :#: (Music a) -- xor

phasor :: (Integer -> Integer) -> Dur -> [IOI] -> [a] -> Music a
phasor ph d iois as = Prim d (Phasor {phase = ph, iois=iois, imps=as})

sequential :: [Music a] -> Music a
sequential []       = error "sequence must not be empty" -- Prim (Imp 0)
sequential [i]      = i
sequential (i : is) = i :+: sequential is

xor :: [Music a] -> Music a
xor []       = error "xor must not be empty"
xor [i]      = i
xor (i : is) = i :#: xor is

foldMusic :: (Dur -> Phasor a -> t) -> (t -> t -> t) -> (t -> t -> t) -> (t -> t -> t) -> Music a -> t
foldMusic prim seq or xor m =
  case m of
    Prim d p  -> prim d p
    m1 :+: m2 -> seq (rec m1) (rec m2)
    m1 :=: m2 -> xor (rec m1) (rec m2)
    m1 :#: m2 -> or (rec m1) (rec m2)
  where
    rec = foldMusic prim seq or xor

-- | Get Music duration
dur :: Music a -> Int
dur = foldMusic prim seq xor or
  where
    prim dur _ = dur
    seq d1 d2 = d1 + d2
    or = max
    xor = max

-- mul :: Float -> Music a -> Music a
-- mul s = foldMusic prim Seq Xor Or
--   where
--     prim (Imp a ioi)         = Prim $ Imp a $ ceiling (s * fromIntegral ioi)
--     prim (Phasor a iois dur) = Prim $ Phasor a (map (ceiling . (*s) . fromIntegral) iois) dur

-- | Change Music duration by a scalar factor
scaleDur :: Float -> Music a -> Music a
scaleDur s = foldMusic prim (:+:) (:=:) (:#:)
  where
    prim dur = Prim (ceiling $ fromIntegral dur * s)


collapse :: C.AudioSample a => Music a -> C.Signal a
collapse (Prim dur ph) = C.phasor dur (iois ph) (imps ph)
collapse (m1 :+: m2)   = C.seq2 (collapse m1) (collapse m2)
collapse (m1 :#: m2)   = C.mix2 (collapse m1) (collapse m2)
collapse (m1 :=: m2)   = C.mix2 (collapse m1) (collapse m2)
