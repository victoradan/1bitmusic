module Music1Bit.Comp
  (
  )
where

import Control.Applicative
import Data.Bits

type Impulse = Bool

-- | Inter-Onset-Interval
type IOI = Integer

newtype Signal a = Signal {sample :: Integer -> a}

-- | Constant 0.
silence :: Signal Impulse
silence = Signal $ const False

-- | Impulse cycle.
cycle :: IOI -> Signal Impulse
cycle ioi = Signal $ \t -> t `mod` ioi == 0

reverse :: Signal a -> Signal a
reverse (Signal f) = Signal $ \t -> f (- t)

-- | Mix two signals together.
mix2 :: Signal Impulse -> Signal Impulse -> Signal Impulse
mix2 (Signal x) (Signal y) = Signal $ \t -> x t `xor` y t

-- | Mix many signals together.
mix :: [Signal Impulse] -> Signal Impulse
mix = foldr mix2 silence

seq :: Integer -> Signal a -> Signal a -> Signal a
seq d (Signal x) (Signal y) = Signal $ \t -> if t < d then x t else y (t - d)

-- | Convolution.
conv :: Integer -> Signal Impulse -> Signal Impulse -> Signal Impulse
conv w (Signal f) (Signal g) = Signal $ \t -> foldr xor False [f (t - m) && g m | m <- [- w .. w]]
