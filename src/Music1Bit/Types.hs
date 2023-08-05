module Music1Bit.Types where

type Time       = Integer
type Tick       = Bool
type Signal     = Time -> Tick
type Frequency  = Integer
type ContSignal = Time -> Integer
type FiniteSignal = Time -> Maybe Tick
type ModSignal  = Time -> Integer
type IOI        = Integer
