{-# LANGUAGE BinaryLiterals #-}

module Main where

import           Music1Bit.Audio       as Audio
import           Music1Bit.Combinators as C
import           Music1Bit.Music       as M


-- | polyrhythmic clusters
pre1 =  M.train (reverse [9, 100 .. 10004]) [(False, True)]
pre2 = M.train (reverse [9, 100 .. 10007]) [(True, True)]
pre3 = M.train (reverse [8, 100 .. 10011]) [(True, False)]
pre = M.ormix [pre1, pre2, pre3]

post1 = M.train [13, 200 .. 12002] [(False, True)]
post2 = M.train [13, 200 .. 12008] [(True, False)]
post3 = M.train [12, 200 .. 12001] [(True, True)]
post = M.ormix [post1, post2, post3]

count = 12000000
-- count = 100000

shiftedPhasor a i n = M.train [i] [a] :+: M.phasor count [n] [a]

c1 = M.xormix $ zipWith (shiftedPhasor (True, True)) [30, 300 ..] [3000 .. 3020]
c2 = M.xormix $ zipWith (shiftedPhasor (True, True)) [50, 550 ..] [5000 .. 5050]
c3 = M.xormix $ zipWith (shiftedPhasor (False, True)) [70, 770 ..] [7000, 7002 .. 7070]
c4 = M.xormix $ zipWith (shiftedPhasor (True, False)) [19, 1230 ..] [19000, 19003 .. 19200]
c = M.xormix [c1, c2, c3, c4]

piece = M.sequential [pre, c, post]
music = C.run $ M.collapse piece

main :: IO ()
main = toWav2 "test.wav" 44100 music
-- main = print pre1
