{-# LANGUAGE BinaryLiterals #-}

module Main where

-- import qualified Data.ByteString.Lazy as BS (concat, putStr, ByteString)
import Music1Bit.Combinators as C
import Music1Bit.Types as C
import Music1Bit.Audio as Audio

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (concat, putStr, ByteString)
import qualified GHC.Int

toInt:: Tick -> GHC.Int.Int32
toInt x = if x then 1 else 0

toInt':: Integer -> Int
toInt' = fromIntegral

-- music = [ C.mix [C.impulse 102, C.impulse 100, C.impulse 101, C.impulse 99] sample | sample <- [0..100000]]
-- music = [ C.mix (map (C.impulse . (*80)) [1, 3..50]) sample | sample <- [0..1000000]]
-- music = [ C.mix (map C.impulse [89..103]) sample | sample <- [0..1000000]]
-- music = [ C.mix (map C.impulse [1000..1010]) sample | sample <- [0..1000000]]

-- music = [ C.mix (map C.impulse [400..410]) sample | sample <- [0..100000]]
-- music = [ C.seq  (C.impulse 1000, 15000) (C.mix (map C.impulse [400..410])) sample | sample <- [0..1000000]]

-- Best
phase = product [400..401] `div` 2
m1 = C.mix (map (C.shift phase . C.impulse) [400..413])
-- pre = C.impulse 400
pre = C.ioi2signal $ reverse [80..1440]
-- music = [ C.seq  (pre, 90000)  m1 n | n <- [0..5000000]]
-- music = [ C.seq  (C.impulse 1000, 15000) (C.mix (map (C.shift (4007*5409 `div` 2) . C.impulse) [4000, 4100..5003])) sample | sample <- [0..1000000]]

-- | two polyrhythm clusters: one around 400, another around 700
c2 = map (C.shift (4000*4410 `div` 2) . C.impulse) [50000, 50002..50010]
c3 = map (C.shift (7000*7400 `div` 2) . C.impulse) [7000,7003..7400]
c4 = map (C.shift (23000*27000 `div` 2) . C.impulse) [20000,20005..20110]
music = [ C.seq  (C.impulse 1000, 15000)  (C.mix (concat [c2, c3, c4])) n | n <- [0..1000000]]

main :: IO ()
-- main = BS.putStr $ BS.concat $ map (encode . (*2^14) . Main.toInt) music
main = Audio.writeMono "foo.wav" $ map ( (*2^24) . Main.toInt) music
