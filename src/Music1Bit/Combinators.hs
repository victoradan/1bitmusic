module Music1Bit.Combinators where

import Data.Bits
import Music1Bit.Types
import Text.Printf (vFmt)
-- import Data.List.NonEmpty

-- |Constant 0.
silence :: Signal
silence = const False

-- |Step function
step :: Integer -> ContSignal
step f t = t `div` f

foo :: IOI -> Signal
foo ioi t = t `mod` ioi == 0

speed:: Integer -> Signal -> Signal
speed n s t = s (t + n)

diff :: (Integer -> Integer) -> (Integer -> Integer)
diff s t = s t - s (t-1)

-- |Impulse-ish 
impulse :: Frequency -> Signal
impulse freq t = t `mod` freq == 0

-- |Mix two signals together by adding amplitudes.
add :: Signal -> Signal -> Signal
add x y t = x t `xor` y t

-- |Mix signals together by adding amplitudes.
mix :: [Signal] -> Signal
mix = foldr add silence

phasor :: [Tick] -> Signal
phasor ts t = cycle ts !! fromInteger t

fm :: ModSignal -> (Integer -> Integer) -> (Integer -> Integer)
fm x y t = y (round (fromIntegral t + (x t)))

fm' :: ModSignal -> Signal -> Signal
fm' m s = fromInt $ diff (fm m (integrate ( toInt s)))

x = integrate . toInt

-- |Integrate
integrate :: (Integer -> Integer) -> (Integer -> Integer)
integrate s t = sum [s t' | t' <- [0..t]]

toInt :: Signal -> (Integer -> Integer)
toInt s t = if s t then 1 else 0

fromInt :: (Integer -> Integer) -> Signal
fromInt s t = s t /= 0

line :: Double -> Double -> ModSignal
line start slope t = start + slope * fromIntegral t 

-- |Calculate an oscillation frequency for a MIDI note number, in an equally tempered scale.
midiNoteToFreq :: (Floating a) => Int -> a
midiNoteToFreq n =
    f0 * (a ** (fromIntegral n - midiA4))
    where
        a = 2 ** (1.0 / 12.0)
        f0 = 440.0 -- A-4 in an ETS is 440 Hz.
        midiA4 = 69 -- A-4 in MIDI is 69.
