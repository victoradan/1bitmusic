module Main where

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M


count = 8000000
freq = 420

pre1 = M.train (reverse [20, 200 .. 13001]) [(True, False)]
pre2 = M.train (reverse [20, 200 .. 13000]) [(False, True)]
pre = M.ormix [pre1, pre2]

post1 = M.train [123, 200 .. 7002] [(False, True)]
post2 = M.train [120, 200 .. 7000] [(True, False)]
post = M.ormix [post1, post2]

p1 = M.phasor count [freq] [(True, False)]

p2a = M.train (replicate (count `div` freq `div` 3)  (freq + 1)) [(False, True)]
p2b = M.train (replicate (count `div` freq `div` 3) (freq - 1)) [(False, True)]
p2c = M.train (replicate (count `div` freq `div` 3) (freq + 1)) [(False, True)]

-- p2b = M.phasor (count `div` 3) [freq - 1] [(False, True)]
-- p2c = M.phasor (count `div` 3) [freq + 1] [(False, True)]
p2 = M.sequential [p2a, p2b, p2c]

music = M.sequential [pre, M.xormix [p1, p2], post]


main :: IO ()
main = toWav2 "tepoz_x.wav" 44100 $ C.run (M.collapse music)
