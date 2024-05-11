module Main where


import Music1Bit.Audio as Audio
import Music1Bit.Combinators as C
import Music1Bit.Types

import LSystem as L

type IR a = [(a, Signal -> Signal)]

interpret :: (Eq a, Show a) => LSys a -> IR a -> Signal -> Signal
interpret (a :. b) r m = interpret a r (interpret b r m)
interpret (a :+ b) r m = C.add (interpret a r m) (interpret b r m)
interpret Id r m = m
interpret (N x) r m = case lookup x r of
  Just f -> f m
  Nothing -> error $ "No interpretation rule for " ++ show x


data LFun = Scale | Shift | Harm | Same deriving (Eq, Ord, Show)

ir :: IR LFun 
ir = [(Scale, C.scale 32), (Shift, C.shift 30), (Same, id), (Harm, \s -> C.add s (C.scale 2 s))]

sc = N Scale
sh = N Shift
harm = N Harm
same = N Same

scsh = sc :+ sh

r1a = Rule sc sh
r1b = Rule sh sc

r2a = Rule sh (scsh :. scsh)
r2b = Rule sh same
r2c = Rule scsh same
r2d = Rule same scsh
r2e = Rule harm sh

g1 = Grammar same (Uni [r1a, r1b, r2a, r2b, r2c, r2d ])


t1 n = interpret (gen replFun g1 6 !! n) ir (C.ioi2signal [390,209])

---

-- c1 = C.mix $ map C.impulse [3000 .. 3020]
-- postDur = 1000
-- music = map c1 [0 .. postDur]
--
music = map (t1 9) [0 .. 200000]

main :: IO ()
main = toWav "tepoz_vii.wav" 44100 music
