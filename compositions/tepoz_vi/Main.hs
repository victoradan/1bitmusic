{-# LANGUAGE BinaryLiterals #-}

module Main where

import Music1Bit.Audio as Audio
import Music1Bit.Combinators as C

-- | polyrhythmic clusters
post1 = C.ioi2signal [11, 200 .. 12002]
post2 = C.ioi2signal [13, 200 .. 12008]
post3 = C.ioi2signal [12, 200 .. 12011]
post = C.mix [post1, post2, post3]

phMul = 100
c1 = C.shift (3000 * phMul) $ C.mix $ map C.impulse [3000 .. 3020]
c2 = C.shift (5000 * phMul) $ C.mix $ map C.impulse [5000 .. 5050]
c3 = C.shift (7000 * phMul) $ C.mix $ map C.impulse [7000, 7002 .. 7070]
c4 = C.shift (20000 * phMul) $ C.mix $ map C.impulse [20000, 20003 .. 20200]

postDur = 380000
bodyDur = 12000000
piece = C.seq' [(postDur, C.reverse post), (bodyDur, C.mix [c1, c2, c3, c4]), (postDur, post)]
music = map piece [0 .. postDur * 2 + bodyDur]

main :: IO ()
main = toWav "tepoz_vi.wav" 44100 music
