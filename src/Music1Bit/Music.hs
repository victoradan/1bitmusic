module Music1Bit.Music where

import           Music1Bit.Combinators as C
import           Music1Bit.Types

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
sequential []       = error "sequence must not be empty" -- Prim (Imp 0)
sequential [i]      = i
sequential (i : is) = i :+: sequential is

parallel :: [Music] -> Music
-- parallel []       = Prim (Imp 0)
parallel []       = error "parallel must not be empty"
parallel [i]      = i
parallel (i : is) = i :=: parallel is

foldMusic prim seq par m =
  case m of
    Prim p    -> prim p
    m1 :+: m2 -> seq (rec m1) (rec m2)
    m1 :=: m2 -> par (rec m1) (rec m2)
  where
    rec = foldMusic prim seq par

collapse :: Music -> Signal
collapse (Prim (Imp ioi))         = C.cycle ioi ioi
collapse (Prim (Phasor iois dur)) = C.newdur dur $ C.seq (map (\ioi -> C.cycle ioi ioi) iois )
collapse (m1 :+: m2)              =  C.seq2 (collapse m1) (collapse m2)
collapse (m1 :=: m2)              = C.mix2 (collapse m1) (collapse m2)
