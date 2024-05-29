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


data LFun = Harm | Same | Up | Down deriving (Eq, Ord, Show)

ir :: IR LFun
ir = [(Harm, \m -> m M.:=: M.phasor (M.dur m) [202, 400, 523]), (Same, id), (Up, M.mul 2.0 ), (Down, M.mul 0.9) ]

sc = N Harm
same = N Same
seq = sc :+ down
up = N Up
down = N Down
down2 = down :. same


r1a = Rule sc up
r1b = Rule same down
r1c = Rule sc Main.seq
r1d = Rule Main.seq same
r1e = Rule up Main.seq
r1f = Rule down sc
r1g = Rule down down
r1h = Rule sc down2
r1i = Rule Main.seq down

g1 = Grammar sc (Uni [r1a, r1b, r1c, r1d, r1e, r1f, r1g, r1h, r1i ])

t1 n = interpret (L.gen L.replFun g1 3 !! n) ir (M.phasor 500 [100, 200, 53])


music = t1 17
signal = M.collapse music

main :: IO ()
-- main = print music
main = toWav "tepoz_xix.wav" 44100 $ C.run signal

-- main :: IO ()
-- main = do
--   print music
--   toWav "tepoz_xix.wav" 44100 $ C.run signal
