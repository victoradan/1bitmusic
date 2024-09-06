{-# LANGUAGE FlexibleInstances #-}

module Music1Bit.Combinators where


-- import           Data.Bits
import qualified Data.Vector     as V
-- import Data.Map.Strict as Map
import           Data.List       as L

import           Music1Bit.Types

type Stereo a = (a, a)
type Mono a = a

class AudioSample a where
    xormix :: a -> a -> a
    ormix :: a -> a -> a
    zero :: a

instance AudioSample (Mono Bool) where
    xormix x a = x /= a
    ormix x a= x || a
    zero = False
instance AudioSample (Stereo Bool) where
    xormix (x, y) (a, b)= (x /= a, y /= b)
    ormix (x, y) (a, b)= (x || a, y || b)
    zero = (False, False)

data Signal a = Signal {sample :: Integer -> a, dur :: Int}

phasor :: AudioSample a => Dur -> [IOI] -> [a] -> Signal a
phasor dur iois as = Signal f dur
    where
        cycleDur = sum iois
        cycleLen = length iois * length as
        integral = init (0 : scanl1 (+) iois)
        integral' = take cycleLen $ Prelude.cycle integral
        as' = take cycleLen $ Prelude.cycle as
        f t =
            case idx of
                Nothing -> zero
                Just n  -> as' !! n
            where
                t' = fromIntegral t `mod` cycleDur
                idx = L.elemIndex t' integral'

-- | Constant signal.
constant :: a -> Dur -> Signal a
constant x = Signal (const x)


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
xormix2 :: AudioSample a => Signal a -> Signal a -> Signal a
xormix2 (Signal xf xd) (Signal yf yd) = Signal (\t -> xf t `xormix` yf t) (max xd yd)

ormix2 :: AudioSample a => Signal a -> Signal a -> Signal a
ormix2 (Signal xf xd) (Signal yf yd) = Signal (\t -> xf t `ormix` yf t) (max xd yd)

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
