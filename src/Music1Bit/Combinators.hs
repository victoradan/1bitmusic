module Music1Bit.Combinators where


-- import           Data.Bits
import qualified Data.Vector     as V

import           Music1Bit.Types

class AudioSample a where
    xor :: a -> a -> a
    or :: a -> a -> a
    zero :: a

instance AudioSample (Bool ,Bool ) where
    xor (x, y) (a, b)= (x /= a, y /= b)
    or (x, y) (a, b)= (x || a, y || b)
    zero = (False, False)

data Signal a = Signal {sample :: Integer -> a, dur :: Int}

ioi2impulses :: AudioSample a => (IOI, a) -> [a]
ioi2impulses (ioi, a)
    | ioi == 0 = []
    | otherwise =  a:replicate (abs ioi - 1) zero

-- | Constant signal.
constant :: a -> Dur -> Signal a
constant x = Signal (const x)

-- | Tick cycle. sampleImpulses
cycle :: AudioSample a => [IOI] -> [a] -> Dur -> Signal a
cycle iois as =
    Signal (\t -> imps !! fromIntegral t)
    -- Signal (\t -> (t `mod` (length imps)) !! imps)
    where
        imps = concatMap ioi2impulses $ zip iois as

seq2 :: Signal a -> Signal a -> Signal a
seq2 (Signal xf xd) (Signal yf yd) =
    Signal (\t -> if t < fromIntegral xd then xf t else yf (t - fromIntegral xd)) (xd + yd)

seq :: [Signal a] -> Signal a
seq ss = Signal (\t -> train V.! (fromIntegral t `mod` len)) len
  where
    train = V.fromList $ concatMap run ss
    len = V.length train

newdur :: Dur -> Signal a -> Signal a
newdur d (Signal f _) = Signal f d

-- | xor two signals together.
mix2 :: AudioSample a => Signal a -> Signal a -> Signal a
mix2 (Signal xf xd) (Signal yf yd) = Signal (\t -> xf t `xor` yf t) (max xd yd)

-- -- | Mix many signals together.
-- mix :: [Signal] -> Signal
-- mix ss = foldr mix2 (silence totalDur) ss
--     where
--         totalDur = maximum $ map dur ss

-- | Render a Signal into a list of Ticks
run :: Signal a -> [a]
run (Signal f d) = map f [0 .. fromIntegral d - 1]

-- -- reverse :: Signal a -> Signal a
-- -- reverse (Signal f) = Signal $ \t -> f (- t)

-- -- -- | Convolution.
-- -- conv :: Integer -> Signal Tick -> Signal Tick -> Signal Tick
-- -- conv w (Signal f) (Signal g) = Signal $ \t -> foldr xor False [f (t - m) && g m | m <- [- w .. w]]
