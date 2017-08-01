import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
{-unitTests = testGroup "Tests" [phasorTests, booleanTests, distributiveTest]-}
unitTests = testGroup "Tests" [phasorTests, booleanTests]

booleanTests = testGroup "BooleanTests" [phasorAndTest, phasorOrTest, phasorXorTest]
phasorTests  = testGroup "PhasorTests"  [runTest, concatTest, convTest, equalizeTest] 

-- # Property Tests
{-distributiveTest = testGroup "distribute" -}
  {-[ testCase "(a % n) & (b % n) == (a & b) % n" $-}
    {-(Phasor [i,o] % 10) & ([i,o,o] % 10) @?= ([i,o] & [i,o,o]) % 10 -- @?= [i,o,o,o,o,o,i,o,o,o]-}
  {-]-}

-- # Functional Tests

{-rotTest = testGroup "rot Phasor"-}
  {-[ testCase "rot phasor by 1" $-}
      {-rot [i,o,o] 1 @?= [o,i,o]-}

  {-, testCase "rot phasor by 2" $-}
      {-rot [i,o,o] 2 @?= [o,o,i]-}

  {-, testCase "rot phasor by length phasor" $-}
      {-rot [i,o,o] 3 @?= [i,o,o]-}

  {-, testCase "rot phasor by -1" $-}
      {-rot [i,o,o] (-1) @?= [o,o,i]-}
  {-]-}

runTest = testGroup "run Phasor"
  [ testCase "run phasor n < length phasor" $
    run (Phasor [i,o,o]) 2 @?= Phasor [i,o]

  , testCase "run phasor n > length phasor" $
    run [i,o,o] 6 @?= [i,o,o,i,o,o]

  , testCase "run n == 0" $
    run [i,o,o] 0 @?= []

  ]

concatTest = testGroup "a ++ b"
  [ testCase "[I,O] ++ [I]" $
    [i,o] ++ [i] @?= [i,o,i]
  ]

phasorAndTest = testGroup "a & b"
  [ testCase "a & b" $
    ([i,i,o,o] & [i,o,i,o]) @?= [i,o,o,o]
  ]

phasorOrTest = testGroup "a \\& b"
  [ testCase "a \\& b" $
    ([i,i,o,o] \& [i,o,i,o]) @?= [i,i,i,o]
  ]

phasorXorTest = testGroup "a # b"
  [ testCase "a # b" $
    ([i,i,o,o] # [i,o,i,o]) @?= [o,i,i,o]
  ]

convTest = testGroup "conv a b"
  [ testCase "conv [i,i,o,o] [i,o,o,i,o,o]" $
    conv [i,i,o,o] [i,o,o,i,o,o] @?= [i,i,o,i,i,o]

  , testCase "conv [i] [i]" $
    conv [i] [i] @?= [i]

  , testCase "conv [i] [i,o]" $
    conv [i] [i,o] @?= [i,o]

  , testCase "conv [o] [i,o]" $
    conv [o] [i,o] @?= [o,o]
  ]

equalizeTest = testGroup "equalize a b"
  [
    testCase "equalize [i] [i,i,i]" $
    equalize [i] [i,i,i] @?= ([i,o,o], [i,i,i])
  ]
