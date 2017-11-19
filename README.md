[Haskell School of Music]: http://haskell.cs.yale.edu/wp-content/uploads/2015/03/HSoM.pdf
[Discrete Time 1-bit Music]: http://victoradan.github.io/discrete-time-1-bit-music.html

# 1 Bit Music Composition Library

This is a Haskell implementation of the minimal Data Types and functions necessary to compose 1-bit music. It is based on a text that formed part of my doctoral thesis on [Discrete Time 1-bit Music].

This code is work-in-progress.

## Domain Models and Algebraic Data Types

The central ADT in the library is the `Comp` (Composition):
```haskell
data Comp a =
      Sq a
    | Ph a
    | Comp a :+: Comp a
    | Comp a :|: Comp a
    | Comp a :&: Comp a
    | Comp a :#: Comp a
    deriving (Show)
```
An entire 1-bit composition (regardless of size or complexity) is described with this recursive data type. 
The data constructors are the following:
- `Sq`: is a plain sequence of IOI (Inter Onset Intervals) or *Ticks*.
- `Ph`: is a *phasor*, or cyclical sequence which is pottentially infinite.
- `:+:`: is a concatenation of two `Comp`s, identical to the `++` operator for lists. 
- `:|:`: is a logical OR operator. It combines two `Comp`s by OR-ing corresponding *Ticks*.
- `:&:`: is a logical AND operator.
- `:#:`: is a logical XOR operator.

## Sample Compositions
Simple combination of periodic sequences with periods 95 to 100:
```haskell
comp1 = foldl1 (:|:) ( map Ph [[i] | i <- [95..100]]) 
```

## Credits and References
- [Haskell School of Music]
- [Discrete Time 1-bit Music]
