module Music1Bit.Types where

type Tick       = Bool
type Frequency  = Integer
type Time       = Integer
type Signal     = Time -> Tick
type ContSignal = Time -> Integer
type FiniteSignal = Time -> Maybe Tick
type ModSignal  = Time -> Double
type IOI        = Integer
