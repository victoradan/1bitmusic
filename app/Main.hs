{-# LANGUAGE BinaryLiterals #-}

module Main where

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (ByteString, concat, putStr)
import Music1Bit.Audio as Audio
import Music1Bit.Combinators as C
import Music1Bit.Types as C


-- | polyrhythmic clusters
pre1 = C.ioi2signal $ reverse [10, 200 .. 12002]
pre2 = C.ioi2signal $ reverse [12, 200 .. 12000]
pre3 = C.ioi2signal $ reverse [11, 200 .. 12001]
pre = C.mix [pre1, pre2, pre3]

post1 = C.ioi2signal [11, 200 .. 12002]
post2 = C.ioi2signal [13, 200 .. 12008]
post3 = C.ioi2signal [12, 200 .. 12011]
post = C.mix [post1, post2, post3]
phMul = 100

c1 = C.shift (3000 * phMul) $ C.mix $ map C.impulse [3000 .. 3020]

c2 = C.shift (5000 * phMul) $ C.mix $ map C.impulse [5000 .. 5050]

c3 = C.shift (7000 * phMul) $ C.mix $ map C.impulse [7000, 7002 .. 7070]

c4 = C.shift (20000 * phMul) $ C.mix $ map C.impulse [20000, 20003 .. 20200]

piece = C.seq' [(380000, pre), (12000000 , C.mix [c1, c2, c3, c4]), (380000, post)]

music = map piece [0 .. 380000*2+12000000]

main :: IO ()
main = toWav "Tepoz.wav" 44100 music
-- main = BS.putStr $ BS.concat $ map (encode . (*2^14) . Main.toInt) music
