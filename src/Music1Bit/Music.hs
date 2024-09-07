module Music1Bit.Music where

import qualified Music1Bit.Combinators as C
import           Music1Bit.Types

data Phasor a = Phasor {iois ::[IOI], imps:: [a]} deriving (Show)

instance Functor Phasor where
  fmap f (Phasor iois imps) = Phasor iois (map f imps)

instance Applicative Phasor where
  pure x = Phasor [] [x]
  (Phasor _ impsX) <*> (Phasor ioisY impsY) = Phasor ioisY [f y | f <- impsX, y <- impsY]

data Music a
  = Prim Dur (Phasor a)
  | (Music a) :+: (Music a) -- sequential
  | (Music a) :=: (Music a)  -- or
  | (Music a) :#: (Music a) deriving (Show) -- xor

train :: [IOI] -> [a] -> Music a
train iois imps = Prim (sum iois) (Phasor iois imps)

phasor :: Dur -> [IOI] -> [a] -> Music a
phasor d iois as = Prim d (Phasor {iois=iois, imps=as})

sequential :: [Music a] -> Music a
sequential []       = error "sequence must not be empty" -- Prim (Imp 0)
sequential [i]      = i
sequential (i : is) = i :+: sequential is

xormix :: [Music a] -> Music a
xormix []       = error "xor must not be empty"
xormix [i]      = i
xormix (i : is) = i :#: xormix is

ormix :: [Music a] -> Music a
ormix []       = error "xor must not be empty"
ormix [i]      = i
ormix (i : is) = i :=: ormix is

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

-- | Scale IOIs
scaleIois :: Float -> Music a -> Music a
scaleIois s = foldMusic prim (:+:) (:=:) (:#:)
  where
    prim dur (Phasor iois as) = Prim dur $ Phasor (map (ceiling . (*s) . fromIntegral) iois) as

-- | Change Music duration by a scalar factor
scaleDur :: Float -> Music a -> Music a
scaleDur s = foldMusic prim (:+:) (:=:) (:#:)
  where
    prim dur = Prim (ceiling $ fromIntegral dur * s)


collapse :: C.AudioSample a => Music a -> C.Signal a
collapse (Prim dur ph) = C.phasor dur  (iois ph) (imps ph)
collapse (m1 :+: m2)   = C.seq2 (collapse m1) (collapse m2)
collapse (m1 :#: m2)   = C.xormix2 (collapse m1) (collapse m2)
collapse (m1 :=: m2)   = C.ormix2 (collapse m1) (collapse m2)

