import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Tests" [seqTests, booleanTests]

booleanTests = testGroup "BooleanTests" [seqAndTest, seqOrTest, seqXorTest]
seqTests     = testGroup "SeqTests"     [rotTest, runTest, concatTest, rollTest, equalizeTest] 

rotTest = testGroup "rot Seq"
  [ testCase "rot seq by 1" $
      rot [i,o,o] 1 @?= [o,i,o]

  , testCase "rot seq by 2" $
      rot [i,o,o] 2 @?= [o,o,i]

  , testCase "rot seq by length seq" $
      rot [i,o,o] 3 @?= [i,o,o]

  , testCase "rot seq by -1" $
      rot [i,o,o] (-1) @?= [o,o,i]
  ]

runTest = testGroup "run Seq"
  [ testCase "run seq n < length seq" $
    run [i,o,o] 2 @?= [i,o]

  , testCase "run seq n > length seq" $
    run [i,o,o] 6 @?= [i,o,o,i,o,o]

  , testCase "run n == 0" $
    run [i,o,o] 0 @?= []

  ]

concatTest = testGroup "a ++ b"
  [ testCase "[I,O] ++ [I]" $
    [i,o] ++ [i] @?= [i,o,i]
  ]

seqAndTest = testGroup "a & b"
  [ testCase "a & b" $
    ([i,i,o,o] & [i,o,i,o]) @?= [i,o,o,o]
  ]

seqOrTest = testGroup "a \\& b"
  [ testCase "a \\& b" $
    ([i,i,o,o] \& [i,o,i,o]) @?= [i,i,i,o]
  ]

seqXorTest = testGroup "a # b"
  [ testCase "a # b" $
    ([i,i,o,o] # [i,o,i,o]) @?= [o,i,i,o]
  ]

rollTest = testGroup "roll a b"
  [ testCase "roll [i,i,o,o] [i,o,o,i,o,o]" $
    roll [i,i,o,o] [i,o,o,i,o,o] @?= [i,i,o,i,i,o]

  , testCase "roll [i] [i]" $
    roll [i] [i] @?= [i]

  , testCase "roll [i] [i,o]" $
    roll [i] [i,o] @?= [i,o]

  , testCase "roll [o] [i,o]" $
    roll [o] [i,o] @?= [o,o]
  ]

equalizeTest = testGroup "equalize a b"
  [
    testCase "equalize [i] [i,i,i]" $
    equalize [i] [i,i,i] @?= ([i,o,o], [i,i,i])
  ]
