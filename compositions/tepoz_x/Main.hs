module Main where

import           System.Random

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M


rotation count period divergence divs = M.xormix [p1, p2]
    where
        chorus = [M.train (replicate (count `div` period `div` divs)  (operator n period divergence)) [(False, True)] | n <- [0 .. divs -1]]
        operator n = if even n then (+) else (-)
        p1 = M.phasor count [period] [(True, False)]
        p2 = M.sequential chorus


pre1 = M.train (reverse [100, 200 .. 8001]) [(True, False)]
pre2 = M.train (reverse [102, 200 .. 8000]) [(True, True)]
pre = M.ormix [pre1, pre2]

post1 = M.train [120, 200 .. 7002] [(False, True)]
post2 = M.train [123, 200 .. 7000] [(True, False)]
post = M.ormix [post1, post2]

noise1 = M.phasor count (700000 : take 181 (randomRs (2,7) (mkStdGen 2))) [(False, True), (False, True), (True, True)]
noise2 = M.phasor count (900011 : take 94 (randomRs (7,31) (mkStdGen 1))) [(True, False), (True, False), (True, True)]
noise3 = M.phasor count (1100023 : take 140 (randomRs (27,41) (mkStdGen 1))) [(True, False), (True, False), (True, True)]

noise = M.xormix [noise1, noise2, noise3]

-- count = 2000000
count = 12000000
period = 400

body = M.ormix [
        rotation count period 3 3,
        rotation count (round $ fromIntegral period / 2 / (7 / 6 )) 1 5,
        -- rotation count (round $ fromIntegral period / 2 / (9 / 8 )) 1,
        rotation count (round $ fromIntegral period  / (3 / 2 ) ) 2 4]
music = M.sequential [pre, M.xormix [body, noise], post]


main :: IO ()
main = toWav2 "tepoz_x.wav" 44100 $ C.run (M.collapse music)
