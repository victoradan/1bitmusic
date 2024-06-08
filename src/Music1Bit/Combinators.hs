module Music1Bit.Combinators where


import           Data.Bits
import qualified Data.Vector     as V

import           Music1Bit.Types

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

-- | Render a Signal into a list of Ticks
run :: Signal -> [Tick]
run (Signal f d) = map f [0 .. fromIntegral d - 1]

-- reverse :: Signal a -> Signal a
-- reverse (Signal f) = Signal $ \t -> f (- t)

-- -- | Convolution.
-- conv :: Integer -> Signal Tick -> Signal Tick -> Signal Tick
-- conv w (Signal f) (Signal g) = Signal $ \t -> foldr xor False [f (t - m) && g m | m <- [- w .. w]]
