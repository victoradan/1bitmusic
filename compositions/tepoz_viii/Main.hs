{-# LANGUAGE BinaryLiterals #-}

module Main where

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M

-- import System.Random

-- | polyrhythmic clusters
-- pre1 =  M.phasor (\t -> t) 400000 (reverse [20, 200 .. 10001]) [True]
pre1 =  M.phasor 400000 [10000, 20000, 30000 ] [True]
-- pre2 = M.sequential $ map M.imp $ reverse [20, 200 .. 10007]
-- pre3 = M.sequential $ map M.imp $ reverse [19, 200 .. 10012]
-- pre = M.parallel [pre1, pre2, pre3]

-- post1 = M.sequential $ map M.imp [13, 200 .. 12002]
-- post2 = M.sequential $ map M.imp [13, 200 .. 12008]
-- post3 = M.sequential $ map M.imp [12, 200 .. 12021]
-- post = M.parallel [post1, post2, post3]

-- -- count = 12000000
-- count = 100000

-- shiftedPhasor :: Int -> Int -> Music
-- shiftedPhasor i n = M.Prim (M.Imp i) :+: M.phasor count [n]

-- c1 = M.parallel $ zipWith shiftedPhasor [30, 300 ..] [3000 .. 3020]
-- c2 = M.parallel $ zipWith shiftedPhasor [50, 550 ..] [5000 .. 5050]
-- c3 = M.parallel $ zipWith shiftedPhasor [70, 770 ..] [7000, 7002 .. 7070]
-- c4 = M.parallel $ zipWith shiftedPhasor [19, 1230 ..] [19000, 19003 .. 19200]
-- c = M.parallel [c1, c2, c3, c4]

-- piece = M.sequential [pre, c, post]

-- signal = M.collapse piece
signal = M.collapse pre1
music = map (C.sample signal) [0 .. fromIntegral (C.dur signal)]

main :: IO ()
-- main = putStr $ show (concatMap C.ioi2impulses [(10, True), (3, True)])
main = toWav "test.wav" 44100 music
