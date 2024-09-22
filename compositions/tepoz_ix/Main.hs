module Main where


import           LSystem               as L
import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M

type IR a b = [(a, Music b -> Music b)]

interpret :: (Eq a, Show a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b) r m = interpret a r (interpret b r m)
interpret (a :+ b) r m = interpret a r m M.:+: interpret b r m
interpret Id r m = m
interpret (N x) r m = case lookup x r of
  Just f  -> f m
  Nothing -> error $ "No interpretation rule for " ++ show x


data LFun = Scale | Harm | Same | Up | Down deriving (Eq, Ord, Show)

flipTuple (a, b) = (not a, not b)

ir :: IR LFun (Bool, Bool)
ir = [
  (Scale, M.scaleDur 1.5),
  (Harm, \m -> m :=: M.phasor (M.dur m) [502, 401] [(False, True)]),
  (Same, M.foldMusic (\d p -> Prim d $ fmap flipTuple p) (:+:) (:=:) (:#:)), -- flip channels
  (Up, M.scaleIois 2),
  (Down, M.scaleIois 0.5)]

sc = N Scale
harm = N Harm
same = N Same
seq = harm :+ down
up = N Up
down = N Down
down2 = down :. same


ra = Rule harm up
rb = Rule same down
rc = Rule harm Main.seq
rd = Rule Main.seq same
re = Rule up Main.seq
rf = Rule down harm
rg = Rule down2 up
rh = Rule harm down2
ri = Rule Main.seq down
rj = Rule sc down2
rk = Rule same sc

g1 = Grammar down (Uni [ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk ])

t1 n = interpret (L.gen L.replFun g1 3 !! n) ir (M.phasor 400 [300, 400] [(True, False)])

pre1 = M.train (reverse [20, 200 .. 13001]) [(False, True)]
pre2 = M.train (reverse [21, 200 .. 13002]) [(True, False)]
pre = M.ormix [pre1, pre2]
post1 = M.train [3, 120 .. 7002] [(True, False)]
post2 = M.train [4, 120 .. 7001] [(False, True)]
post = M.ormix [post1, post2]

music = M.sequential [pre, t1 17, post]

main :: IO ()
main = toWav2 "tepoz_ix.wav" 44100 $ C.run $ M.collapse music
