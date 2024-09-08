module Main where

import           System.Random

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M


count = 9000000
freq = 450

pre1 = M.train (reverse [100, 200 .. 8001]) [(True, False)]
pre2 = M.train (reverse [102, 200 .. 8000]) [(False, True)]
pre = M.ormix [pre1, pre2]

post1 = M.train [120, 200 .. 7002] [(False, True)]
post2 = M.train [123, 200 .. 7000] [(True, False)]
post = M.ormix [post1, post2]

p1 = M.phasor count [freq] [(True, False)]

p2a = M.train (replicate (count `div` freq `div` 3)  (freq + 1)) [(False, True)]
p2b = M.train (replicate (count `div` freq `div` 3) (freq - 1)) [(False, True)]
p2c = M.train (replicate (count `div` freq `div` 3) (freq + 1)) [(False, True)]
-- p2b = M.phasor (count `div` 3) [freq - 1] [(False, True)]
-- p2c = M.phasor (count `div` 3) [freq + 1] [(False, True)]
p2 = M.sequential [p2a, p2b, p2c]

noise1 = M.phasor count (700000 : take 600 (randomRs (2,7) (mkStdGen 2))) [(False, True), (False, True), (True, True)]
noise2 = M.phasor count (500011 : take 100 (randomRs (4,21) (mkStdGen 1))) [(True, False), (True, False), (True, True)]

music = M.sequential [pre, M.xormix [p1, p2, noise1, noise2], post]


main :: IO ()
main = toWav2 "tepoz_x.wav" 44100 $ C.run (M.collapse music)
