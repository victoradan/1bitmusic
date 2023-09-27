module Music1Bit.Combinators where

import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.MemoTrie
import Music1Bit.Types
-- import Data.List.NonEmpty

-- | Constant 0.
silence :: Signal
silence = const False

-- | Step function
step :: Integer -> ContSignal
step f t = t `div` f

diff :: (Integer -> Integer) -> (Integer -> Integer)
diff s t = s t - s (t -1)

train :: V.Vector Tick -> Signal
train ticks t = ticks V.! (fromInteger t `mod` V.length ticks)

ioi2signal :: [IOI] -> Signal
ioi2signal iois = train $ V.fromList $ iois2ticks iois

iois2ticks :: [IOI] -> [Tick]
iois2ticks = concatMap ioi2ticks

ioi2ticks :: IOI -> [Tick]
ioi2ticks ioi
  | ioi == 0 = []
  | otherwise = True : replicate (fromInteger ioi -1) False

-- | Impulse-ish
impulse :: IOI -> Signal
impulse ioi t = t `mod` ioi == 0

impulseM = memo impulse

shift :: Integer -> Signal -> Signal
shift int s t = s (t + int)

-- | Mix two signals together by adding amplitudes.
add :: Signal -> Signal -> Signal
add x y t = x t `xor` y t

-- | Mix signals together by adding amplitudes.
mix :: [Signal] -> Signal
mix = foldr add silence

seq :: (Signal, Integer) -> Signal -> Signal
seq (x, d) y t = if t < d then x t else y (t - d)

inverse :: Signal -> Signal
inverse x t = not $ x t

fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- ramp :: Integer -> Integer -> Integer -> Signal
-- ramp interval start end t = [start, interval..end] !! t

-- phasor :: [Tick] -> Signal
-- phasor ts t = cycle ts !! fromInteger t

-- phasorMod :: [Integer] -> ModSignal
-- phasorMod ts t = cycle ts !! fromInteger t

-- fm :: ModSignal -> Signal -> Signal
-- fm x y t = y (t + x t)

-- | Integrate
toInt :: Signal -> (Integer -> Integer)
toInt s t = if s t then 1 else 0

fromInt :: (Integer -> Integer) -> Signal
fromInt s t = s t /= 0

-- | Calculate an oscillation frequency for a MIDI note number, in an equally tempered scale.
midiNoteToFreq :: (Floating a) => Int -> a
midiNoteToFreq n =
  f0 * (a ** (fromIntegral n - midiA4))
  where
    a = 2 ** (1.0 / 12.0)
    f0 = 440.0 -- A-4 in an ETS is 440 Hz.
    midiA4 = 69 -- A-4 in MIDI is 69.
