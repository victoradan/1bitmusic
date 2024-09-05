module Music1Bit.Music where

import qualified Music1Bit.Combinators as C
import           Music1Bit.Types

-- TODO get rid of Rest? an Imp with a=`0` is a rest
data Primitive a = Phasor [(IOI, a)] deriving (Show)

-- TODO? put Dur at Music level?
data Music a
  = Prim Dur (Primitive a)
  | (Music a) :+: (Music a) -- sequential
  | (Music a) :=: (Music a)  -- or
  | (Music a) :#: (Music a) -- xor
  deriving (Show)

phasor :: Dur -> [IOI] -> [a] -> Music a
phasor d iois as = Prim d (Phasor $ zip iois as)

sequential :: [Music a] -> Music a
sequential []       = error "sequence must not be empty" -- Prim (Imp 0)
sequential [i]      = i
sequential (i : is) = i :+: sequential is

xor :: [Music a] -> Music a
xor []       = error "xor must not be empty"
xor [i]      = i
xor (i : is) = i :#: xor is

foldMusic :: (Dur -> Primitive a -> t) -> (t -> t -> t) -> (t -> t -> t) -> (t -> t -> t) -> Music a -> t
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
    prim dur (Phasor items) = Prim (ceiling $ fromIntegral dur * s) $ Phasor items

collapse :: C.AudioSample a => Music a -> C.Signal a
collapse (Prim dur (Phasor imps)) = C.cycle iois as dur
  where
    (iois, as) = unzip imps
collapse ( m1 :+: m2  )           =  C.seq2 (collapse m1) (collapse m2)
collapse (m1 :#: m2 )             = C.mix2 (collapse m1) (collapse m2)
collapse (m1 :=: m2)              = C.mix2 (collapse m1) (collapse m2)
