{-# LANGUAGE BinaryLiterals #-}

module Main where

-- import qualified Data.ByteString.Lazy as BS (concat, putStr, ByteString)

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (ByteString, concat, putStr)
import qualified GHC.Int
import Music1Bit.Audio as Audio
import Music1Bit.Combinators as C
import Music1Bit.Types as C

toInt :: Tick -> GHC.Int.Int32
toInt x = if x then 1 else 0

-- music = [ C.mix [C.impulse 102, C.impulse 100, C.impulse 101, C.impulse 99] sample | sample <- [0..100000]]
-- music = [ C.mix (map (C.impulse . (*80)) [1, 3..50]) sample | sample <- [0..1000000]]
-- music = [ C.mix (map C.impulse [89..103]) sample | sample <- [0..1000000]]
-- music = [ C.mix (map C.impulse [1000..1010]) sample | sample <- [0..1000000]]

-- music = [ C.mix (map C.impulse [400..410]) sample | sample <- [0..100000]]
-- music = [ C.seq  (C.impulse 1000, 15000) (C.mix (map C.impulse [400..410])) sample | sample <- [0..1000000]]

-- Best
-- phase = product [400..401] `div` 2
-- m1 = C.mix (map (C.shift phase . C.impulse) [400..413])
-- pre = C.impulse 400
-- pre = C.ioi2signal $ reverse [80..1440]
-- music = [ C.seq  (pre, 90000)  m1 n | n <- [0..5000000]]
-- music = [ C.seq  (C.impulse 1000, 15000) (C.mix (map (C.shift (4007*5409 `div` 2) . C.impulse) [4000, 4100..5003])) sample | sample <- [0..1000000]]

-- | polyrhythmic clusters:
pre = C.ioi2signal $ reverse [10, 200 .. 10000]

phMul = 100

c1 = C.shift (3000 * phMul) $ C.mix $ map C.impulse [3000 .. 3020]

c2 = C.shift (5000 * phMul) $ C.mix $ map C.impulse [5000 .. 5050]

c3 = C.shift (7000 * phMul) $ C.mix $ map C.impulse [7000, 7002 .. 7070]

c4 = C.shift (20000 * phMul) $ C.mix $ map C.impulse [20000, 20003 .. 20200]

call = C.seq (pre, 260000) (C.mix [c1, c2, c3, c4])

music = map call [0 .. 10000000]

main :: IO ()
-- main = BS.putStr $ BS.concat $ map (encode . (*2^14) . Main.toInt) music
main = Audio.writeMono 44100 "foo.wav" $ map ((* 2 ^ 28) . Main.toInt) music
