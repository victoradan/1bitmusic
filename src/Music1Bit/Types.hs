module Music1Bit.Types where

type Time       = Integer
type IOI        = Integer
type Tick       = Bool
type Signal     = Time -> Tick
type ModSignal  = Time -> Integer
