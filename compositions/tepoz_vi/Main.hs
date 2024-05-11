{-# LANGUAGE BinaryLiterals #-}

module Main where

import Music1Bit.Audio as Audio
import Music1Bit.Music as M
import qualified GHC.Base as M

-- | polyrhythmic clusters
post1 = M.sequential $ map M.imp [11, 200 .. 12002]
post2 = M.sequential $ map M.imp [13, 200 .. 12008]
post3 = M.sequential $ map M.imp [12, 200 .. 12011]
post = post1 :=: post2 :=: post3

-- phMul = 100
-- c1 = M.shift (3000 * phMul) $ M.mix $ map M.impulse [3000 .. 3020]
-- c2 = M.shift (5000 * phMul) $ M.mix $ map M.impulse [5000 .. 5050]
-- c3 = M.shift (7000 * phMul) $ M.mix $ map M.impulse [7000, 7002 .. 7070]
-- c4 = M.shift (19000 * phMul) $ M.mix $ map M.impulse [19000, 19003 .. 19200]

-- postDur = 380000
-- bodyDur = 12000000
-- piece = M.seq' [(postDur, M.reverse post), (bodyDur, M.mix [c1, c2, c3, c4]), (postDur, post)]
-- music = map piece [0 .. postDur * 2 + bodyDur]
music = M.collapse post

main :: IO ()
main = toWav "test.wav" 44100 music
