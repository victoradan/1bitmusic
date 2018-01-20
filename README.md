[Discrete Time 1-bit Music]: http://victoradan.github.io/discrete-time-1-bit-music.html
[Haskell School of Music]: http://haskell.cs.yale.edu/wp-content/uploads/2015/03/HSoM.pdf

# 1 Bit Music Composition Library

This is a Haskell implementation of the minimal Data Types and functions necessary to compose 1-bit music. It is based on a text that was part of my doctoral thesis on [Discrete Time 1-bit Music].

This code is work-in-progress.

## Domain Models and Algebraic Data Types

The central ADT in the library is the `Comp` (Composition):
```haskell
data Comp =
      Sq [IOI]
    | Ph [IOI] Steps
    | Comp :+: Comp 
    | Comp :|: Comp
    | Comp :&: Comp 
    | Comp :#: Comp 
```
An entire 1-bit composition (regardless of size or complexity) is composed with this recursive data type. 
The data constructors are the following:
- `Sq`: is a plain sequence of IOI (Inter Onset Intervals).
- `Ph`: is a *phasor*, or cyclical sequence which is pottentially infinite. `Steps` is the number of *Ticks* to execute.
- `:+:`: is a concatenation of two `Comp`s, identical to the `++` operator for lists. 
- `:|:`: is a logical OR operator. It combines two `Comp`s by OR-ing corresponding *Ticks*.
- `:&:`: is a logical AND operator.
- `:#:`: is a logical XOR operator.

## Sample Compositions
Simple combination of periodic sequences with periods 95 to 100:
```haskell
composition = foldl1 (:|:) [Ph [i] steps | i <- [95..100], let steps = 30000]
```
To write the composition to disk as an audio `wav` file:
```
wavify composition 1000 "test.wav"
```
`wavify` takes a `Comp`, and `Int` specifying the Tick rate, and the name of the file; ir returns IO ().

## Credits and References
- [Discrete Time 1-bit Music]
- [Haskell School of Music]
