{-# LANGUAGE BinaryLiterals #-}

module Main where

-- import qualified Data.ByteString.Lazy as BS (concat, putStr, ByteString)
import Music1Bit.Combinators as C
import Music1Bit.Types as C

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (concat, putStr, ByteString)

toInt:: Tick -> Int
toInt x = if x then 1 else 0

toInt':: Integer -> Int
toInt' = fromIntegral

-- music = [ C.mix [C.impulse 102, C.impulse 100, C.impulse 101, C.impulse 99] sample | sample <- [0..100000]]
-- music = [ C.mix (map (C.impulse . (*80)) [1, 3..50]) sample | sample <- [0..1000000]]
-- music = [ C.mix (map C.impulse [89..103]) sample | sample <- [0..1000000]]
-- music = [ C.mix (map C.impulse [1000..1010]) sample | sample <- [0..1000000]]

-- music = [ C.mix (map C.impulse [400..410]) sample | sample <- [0..100000]]
-- music = [ C.seq  (C.impulse 1000, 15000) (C.mix (map C.impulse [400..410])) sample | sample <- [0..1000000]]
music = [ C.seq  (C.impulse 1000, 15000) (C.mix (map (C.shift (407*409 `div` 2) . C.impulse) [400..410])) sample | sample <- [0..1000000]]

main :: IO ()
main = BS.putStr $ BS.concat $ map (encode . (*2^9) . Main.toInt) music

-- main = do
--     let delta = 1
--     let s = C.step 10
--     let samples = 100
--     print $ [s t | t <- [0,delta..samples]]
    -- print $ [C.line 0 1 t | t <-[0, delta..samples]]
    -- print $ [C.fm (C.phasorMod [1,2,3] ) s t | t <- [0,delta..samples]]
    -- let music = [C.diff s t | t <- [0,delta..samples]]
    -- let music = [C.diff (C.fm (C.line 0 1.01) s) t | t <- [0,delta..samples]]
    -- print music
    -- BS.putStr $ BS.concat $ map (encode . (*2^12) . toInt' ) music
-- main = do
--     print $ [C.toInt sig t | t <- [0..100]]
--     print $ [(C.diff $ C.integrate $ C.toInt sig ) t | t <- [0..100]]
--     print $ [C.toInt ( C.fm' (C.line 0 0.1) sig) t | t <- [0..100]]
