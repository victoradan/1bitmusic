module Music1Bit.Combinators where

import Data.Bits
import Data.MemoTrie
import qualified Data.Vector.Unboxed as V

import Music1Bit.Types

type Time       = Integer
type Signal = Time -> Maybe Tick

-- type Signal2 a = () -> (a, Signal2)

-- import Data.List.NonEmpty

-- | Constant 0.
silence :: Signal
silence = const $ Just False

-- train :: V.Vector Tick -> Signal
-- train ticks t = ticks V.! idx
--   where
--     len = V.length ticks
--     idx = (len + fromInteger t) `mod` len
-- 
-- ioi2signal :: [IOI] -> Signal
-- ioi2signal iois = train $ V.fromList $ iois2ticks iois
-- 
-- iois2ticks :: [IOI] -> [Tick]
-- iois2ticks = concatMap ioi2ticks
-- 
-- ioi2ticks :: IOI -> [Tick]
-- ioi2ticks ioi
--   | ioi == 0 = []
--   | otherwise = True : replicate (fromInteger ioi -1) False
-- 
-- -- | Impulse-ish
-- impulse :: IOI -> Signal
-- impulse ioi t = (ioi + t) `mod` ioi == 0
-- 
-- impulseM = memo impulse
-- 
-- shift :: Integer -> Signal -> Signal
-- shift int s t = s (t + int)
-- 
-- -- | Mix two signals together by adding amplitudes.
-- add :: Signal -> Signal -> Signal
-- add x y t = x t `xor` y t
-- 
-- -- | Mix signals together by adding amplitudes.
-- mix :: [Signal] -> Signal
-- mix = foldr add silence
-- 
-- seq :: (Signal, Integer) -> Signal -> Signal
-- seq (x, d) y t = if t < d then x t else y (t - d)
-- 
-- seq' :: [(Integer, Signal)] -> Signal
-- seq' ss = train $ V.fromList $ concatMap run ss
--   where
--     run :: (Integer, Signal) -> [Tick]
--     run (d, s) = map s [0 .. d]
-- 
-- inverse :: Signal -> Signal
-- inverse x t = not $ x t
-- 
-- reverse :: Signal -> Signal
-- reverse s t = s (-t)
-- 
-- fib n = fibs !! n
--   where
--     fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- 
-- -- fm :: ModSignal -> Signal -> Signal
-- -- fm x y t = y (t + x t)
-- 
-- -- | Integrate
-- toInt :: Signal -> (Integer -> Integer)
-- toInt s t = if s t then 1 else 0
-- 
-- fromInt :: (Integer -> Integer) -> Signal
-- fromInt s t = s t /= 0
-- 
-- -- | Calculate an oscillation frequency for a MIDI note number, in an equally tempered scale.
-- midiNoteToFreq :: (Floating a) => Int -> a
-- midiNoteToFreq n =
--   f0 * (a ** (fromIntegral n - midiA4))
--   where
--     a = 2 ** (1.0 / 12.0)
--     f0 = 440.0 -- A-4 in an ETS is 440 Hz.
--     midiA4 = 69 -- A-4 in MIDI is 69.
