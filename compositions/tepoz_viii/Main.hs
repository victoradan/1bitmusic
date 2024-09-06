{-# LANGUAGE BinaryLiterals #-}

module Main where

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M

-- import System.Random

-- | polyrhythmic clusters
pre1 =  M.train (reverse [20, 200 .. 10001]) [True]
pre2 = M.train (reverse [20, 200 .. 10007]) [True]
pre3 = M.train (reverse [19, 200 .. 10012]) [True]
pre = M.ormix [pre1, pre2, pre3]

post1 = M.train [13, 200 .. 12002] [True]
post2 = M.train [13, 200 .. 12008] [True]
post3 = M.train [12, 200 .. 12021] [True]
post = M.ormix [post1, post2, post3]

count = 12000000
-- count = 1000000

shiftedPhasor :: Int -> Int -> Music Bool
shiftedPhasor i n = M.train [i] [True] :+: M.phasor count [n] [True]

c1 = M.xormix $ zipWith shiftedPhasor [30, 300 ..] [3000 .. 3020]
c2 = M.xormix $ zipWith shiftedPhasor [50, 550 ..] [5000 .. 5050]
c3 = M.xormix $ zipWith shiftedPhasor [70, 770 ..] [7000, 7002 .. 7070]
c4 = M.xormix $ zipWith shiftedPhasor [19, 1230 ..] [19000, 19003 .. 19200]
c = M.xormix [c1, c2, c3, c4]

piece = M.sequential [pre, c, post]

signal = M.collapse piece
-- signal = M.collapse pre
music = map (C.sample signal) [0 .. fromIntegral (C.dur signal)]

main :: IO ()
-- main = putStr $ show (concatMap C.ioi2impulses [(10, True), (3, True)])
main = toWav "test.wav" 44100 music
