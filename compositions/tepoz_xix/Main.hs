module Main where


import           LSystem               as L
import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M

type IR a = [(a, Music -> Music)]

interpret :: (Eq a, Show a) => LSys a -> IR a -> Music -> Music
interpret (a :. b) r m = interpret a r (interpret b r m)
interpret (a :+ b) r m = interpret a r m M.:+: interpret b r m
interpret Id r m = m
interpret (N x) r m = case lookup x r of
  Just f  -> f m
  Nothing -> error $ "No interpretation rule for " ++ show x


data LFun = Scale | Harm | Same | Up | Down deriving (Eq, Ord, Show)

-- TODO
-- Add more M.muls with diff values

ir :: IR LFun
ir = [
  (Scale, M.scale 1.5),
  (Harm, \m -> m M.:=: M.phasor (M.dur m) [502, 401]),
  (Same, id),
  (Up, M.mul 2),
  (Down, M.mul 0.5)]

sc = N Scale
harm = N Harm
same = N Same
seq = harm :+ down
up = N Up
down = N Down
down2 = down :. same


r1a = Rule harm up
r1b = Rule same down
r1c = Rule harm Main.seq
r1d = Rule Main.seq same
r1e = Rule up Main.seq
r1f = Rule down harm
r1g = Rule down2 up
r1h = Rule harm down2
r1i = Rule Main.seq down
r1j = Rule sc down2
r1k = Rule same sc

g1 = Grammar down (Uni [r1a, r1b, r1c, r1d, r1e, r1f, r1g, r1h, r1i, r1j, r1k ])

t1 n = interpret (L.gen L.replFun g1 3 !! n) ir (M.phasor 400 [300, 400])

pre = M.sequential $ map M.imp $ reverse [20, 200 .. 13001]
post = M.sequential $ map M.imp [13, 100 .. 9002]

music = M.sequential [pre, t1 17, post]

main :: IO ()
main = toWav "tepoz_xix.wav" 44100 $ C.run $ M.collapse music
