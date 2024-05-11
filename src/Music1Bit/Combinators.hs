module Music1Bit.Combinators where

import           Music1Bit.Types

import           Data.Bits
import qualified Data.Vector     as V


data Signal = Signal {sample :: Integer -> Tick, dur :: Int}

-- | Constant 0.
silence :: Dur -> Signal
silence = Signal (const False)

-- | Tick cycle.
cycle :: IOI -> Dur -> Signal
cycle ioi =
    if ioi < 1 then error "ioi must be positive" else Signal (\t -> t `mod` fromIntegral ioi == 0)

seq2 :: Signal -> Signal -> Signal
seq2 (Signal xf xd) (Signal yf yd) =
    Signal (\t -> if t < fromIntegral xd then xf t else yf (t - fromIntegral xd)) (xd + yd)

seq :: [Signal] -> Signal
seq ss = Signal (\t -> train V.! (fromIntegral t `mod` len)) len
  where
    train = V.fromList $ concatMap run ss
    len = V.length train

newdur :: Dur -> Signal -> Signal
newdur d (Signal f _) = Signal f d

-- | Mix two signals together.
mix2 :: Signal -> Signal -> Signal
mix2 (Signal xf xd) (Signal yf yd) = Signal (\t -> xf t `xor` yf t) (max xd yd)

-- | Mix many signals together.
mix :: [Signal] -> Signal
mix ss = foldr mix2 (silence totalDur) ss
    where
        totalDur = maximum $ map dur ss

run :: Signal -> [Tick]
run (Signal f d) = map f [0 .. fromIntegral d]

-- train :: V.Vector Tick -> Signal
-- train ticks t d =
--   let
--     len = V.length ticks
--     idx = (len + fromInteger t) `mod` len
--   in Signal (\t -> (ticks V.! idx)) d

-- reverse :: Signal a -> Signal a
-- reverse (Signal f) = Signal $ \t -> f (- t)


-- seq :: Integer -> Signal a -> Signal a -> Signal a
-- seq d (Signal x) (Signal y) = Signal $ \t -> if t < d then x t else y (t - d)

-- -- | Convolution.
-- conv :: Integer -> Signal Tick -> Signal Tick -> Signal Tick
-- conv w (Signal f) (Signal g) = Signal $ \t -> foldr xor False [f (t - m) && g m | m <- [- w .. w]]


-------------------------------------------------------------
-- import Data.Bits
-- import Data.MemoTrie
-- import qualified Data.Vector.Unboxed as V

-- import Music1Bit.Types

-- type Time       = Integer
-- type Signal = Time -> Maybe Tick

-- -- type Signal2 a = () -> (a, Signal2)

-- -- import Data.List.NonEmpty

-- -- | Constant 0.
-- silence :: Signal
-- silence = const $ Just False

-- train :: V.Vector Tick -> Signal
-- train ticks t d =
--   let
--     len = V.length ticks
--     idx = (len + fromInteger t) `mod` len
--   in Signal (\t -> (ticks V.! idx)) d

-- ioi2signal :: [IOI] -> Signal
-- ioi2signal iois = train $ V.fromList $ iois2ticks iois

-- iois2ticks :: [IOI] -> [Tick]
-- iois2ticks = concatMap ioi2ticks

-- ioi2ticks :: IOI -> [Tick]
-- ioi2ticks ioi
--   | ioi == 0 = []
--   | otherwise = True : replicate (ioi -1) False

-- -- -- | Tick-ish
-- -- impulse :: IOI -> Signal
-- -- impulse ioi t = (ioi + t) `mod` ioi == 0
-- --
-- -- impulseM = memo impulse
-- --
-- -- shift :: Integer -> Signal -> Signal
-- -- shift int s t = s (t + int)
-- --
-- -- -- | Mix two signals together by adding amplitudes.
-- -- add :: Signal -> Signal -> Signal
-- -- add x y t = x t `xor` y t
-- --
-- -- -- | Mix signals together by adding amplitudes.
-- -- mix :: [Signal] -> Signal
-- -- mix = foldr add silence
-- --
-- -- seq :: (Signal, Integer) -> Signal -> Signal
-- -- seq (x, d) y t = if t < d then x t else y (t - d)
-- --
-- -- seq' :: [(Integer, Signal)] -> Signal
-- -- seq' ss = train $ V.fromList $ concatMap run ss
-- --   where
-- --     run :: (Integer, Signal) -> [Tick]
-- --     run (d, s) = map s [0 .. d]
-- --
-- -- inverse :: Signal -> Signal
-- -- inverse x t = not $ x t
-- --
-- -- reverse :: Signal -> Signal
-- -- reverse s t = s (-t)
-- --
-- -- fib n = fibs !! n
-- --   where
-- --     fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- --
-- -- -- fm :: ModSignal -> Signal -> Signal
-- -- -- fm x y t = y (t + x t)
-- --
-- -- -- | Integrate
-- -- toInt :: Signal -> (Integer -> Integer)
-- -- toInt s t = if s t then 1 else 0
-- --
-- -- fromInt :: (Integer -> Integer) -> Signal
-- -- fromInt s t = s t /= 0
-- --
-- -- -- | Calculate an oscillation frequency for a MIDI note number, in an equally tempered scale.
-- -- midiNoteToFreq :: (Floating a) => Int -> a
-- -- midiNoteToFreq n =
-- --   f0 * (a ** (fromIntegral n - midiA4))
-- --   where
-- --     a = 2 ** (1.0 / 12.0)
-- --     f0 = 440.0 -- A-4 in an ETS is 440 Hz.
-- --     midiA4 = 69 -- A-4 in MIDI is 69.
