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


data LFun = Harm | Seq | Same deriving (Eq, Ord, Show)

ir :: IR LFun
ir = [(Harm, \m -> m M.:=: M.phasor (M.dur m) [100, 200]), (Same, id) ]

sc = N Harm
same = N Same
seq = sc :+ same


r1a = Rule sc same
r1b = Rule same sc
r1c = Rule sc Main.seq
r1d = Rule Main.seq same


g1 = Grammar sc (Uni [r1a, r1b, r1c, r1d ])

t1 n = interpret (L.gen L.replFun g1 2 !! n) ir (M.phasor 1000 [100, 200 .. 1000])


music = t1 33
signal = M.collapse music

main :: IO ()
-- main = print music
main = toWav "tepoz_xix.wav" 44100 $ C.run signal

-- main :: IO ()
-- main = do
--   print music
--   toWav "tepoz_xix.wav" 44100 $ C.run signal
