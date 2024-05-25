module Main where

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M


count = 1000000

shiftedPhasor :: Int -> Int -> Music
shiftedPhasor i n = M.Prim (M.Imp i) :+: M.phasor count [n]

c1 = M.parallel $ zipWith shiftedPhasor [30, 1000 ..] [19000 .. 19019]
--c1 = M.parallel $ zipWith shiftedPhasor [30, 1000 ..] [3000 .. 3020]

--
signal = M.collapse c1
music = map (C.sample signal) [0 .. fromIntegral (C.dur signal)]


main :: IO ()
main = toWav "tepoz_x.wav" 44100 music
