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


data LFun = Harm | Same deriving (Eq, Ord, Show)

ir :: IR LFun
ir = [(Harm, \m -> m M.:+: M.phasor 1000 [10, 10, 2, 12]), (Same, id) ]

sc = N Harm
same = N Same


r1a = Rule sc same
r1b = Rule same sc


g1 = Grammar same (Uni [r1a, r1b ])

t1 n = interpret (L.gen L.replFun g1 6 !! n) ir (M.phasor 1000 [132,321])


music = M.collapse (t1 115)
signal = C.run music

main :: IO ()
main = toWav "tepoz_xix.wav" 44100 signal
