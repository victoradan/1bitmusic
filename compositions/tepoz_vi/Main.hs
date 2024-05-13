{-# LANGUAGE BinaryLiterals #-}

module Main where

import Music1Bit.Audio as Audio
import Music1Bit.Music as M
-- import System.Random


-- | polyrhythmic clusters
post1 = M.sequential $ map M.imp [11, 200 .. 12002]
post2 = M.sequential $ map M.imp [13, 200 .. 12008]
post3 = M.sequential $ map M.imp [12, 200 .. 12011]
post = M.parallel [post1, post2, post3]


shiftedPhasor :: Int -> Int -> Music
shiftedPhasor i n = M.Prim (M.Imp i) :+: M.phasor count [n]

count = 1000000
c1 = M.parallel $ zipWith shiftedPhasor [30, 400 .. ] [3000 .. 3020]
c2 = M.parallel $ zipWith shiftedPhasor [50, 650 .. ] [5000 .. 5050]
c3 = M.parallel $ zipWith shiftedPhasor [70, 870 .. ] [7000, 7002 .. 7070]
c4 = M.parallel $ zipWith shiftedPhasor [19, 230 .. ] [19000, 19003 .. 19200]
c = M.parallel [c1, c2, c3, c4]
-- postDur = 380000
-- bodyDur = 12000000
piece = M.sequential [c,post]--  [(postDur, M.reverse post), (bodyDur, M.mix [c1, c2, c3, c4]), (postDur, post)]
-- music = map piece [0 .. postDur * 2 + bodyDur]
music = M.collapse c

main :: IO ()
-- main = putStr $ show piece
main = toWav "test.wav" 44100 music
