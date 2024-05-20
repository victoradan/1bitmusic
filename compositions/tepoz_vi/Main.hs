{-# LANGUAGE BinaryLiterals #-}

module Main where

import           Music1Bit.Audio as Audio
import           Music1Bit.Music as M

-- import System.Random

-- | polyrhythmic clusters
pre1 = M.sequential $ map M.imp $ reverse [11, 200 .. 10001]

pre2 = M.sequential $ map M.imp $ reverse [10, 200 .. 10007]

pre3 = M.sequential $ map M.imp $ reverse [9, 200 .. 10011]

pre = M.parallel [pre1, pre2, pre3]

post1 = M.sequential $ map M.imp [11, 200 .. 12002]

post2 = M.sequential $ map M.imp [13, 200 .. 12008]

post3 = M.sequential $ map M.imp [12, 200 .. 12011]

post = M.parallel [post1, post2, post3]

-- count = 12000000
count = 10000000

shiftedPhasor :: Int -> Int -> Music
shiftedPhasor i n = M.Prim (M.Imp i) :+: M.phasor count [n]

c1 = M.parallel $ zipWith shiftedPhasor [30, 300 ..] [3000 .. 3020]

c2 = M.parallel $ zipWith shiftedPhasor [50, 550 ..] [5000 .. 5050]

c3 = M.parallel $ zipWith shiftedPhasor [70, 770 ..] [7000, 7002 .. 7070]

c4 = M.parallel $ zipWith shiftedPhasor [19, 1230 ..] [19000, 19003 .. 19200]

c = M.parallel [c1, c2, c3, c4]

piece = M.sequential [pre, c, post]

music = M.collapse piece

main :: IO ()
-- main = putStr $ show music
main = toWav "test.wav" 44100 music
