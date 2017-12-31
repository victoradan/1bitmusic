import Test.Tasty
import Test.Tasty.HUnit

import Music1Bit.Comp (run)
import Music1Bit.Tick (Tick(..))

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Tests" [runTest]


runTest = testGroup "run Phasor"
  [ testCase "run n < length" $
    run 2 [I,O,O]  @?= [I,O]

  , testCase "run n > length" $
    run 6 [I,O,O] @?= [I,O,O,I,O,O]

  , testCase "run n == 0" $
    run 0 [I,O,O] @?= []

  ]

{-concatTest = testGroup "a ++ b"-}
  {-[ testCase "[I,O] ++ [I]" $-}
    {-[i,o] ++ [i] @?= [i,o,i]-}
  {-]-}

{-phasorAndTest = testGroup "a & b"-}
  {-[ testCase "a & b" $-}
    {-([i,i,o,o] & [i,o,i,o]) @?= [i,o,o,o]-}
  {-]-}

{-phasorOrTest = testGroup "a \\& b"-}
  {-[ testCase "a \\& b" $-}
    {-([i,i,o,o] \& [i,o,i,o]) @?= [i,i,i,o]-}
  {-]-}

{-phasorXorTest = testGroup "a # b"-}
  {-[ testCase "a # b" $-}
    {-([i,i,o,o] # [i,o,i,o]) @?= [o,i,i,o]-}
  {-]-}

{-convTest = testGroup "conv a b"-}
  {-[ testCase "conv [i,i,o,o] [i,o,o,i,o,o]" $-}
    {-conv [i,i,o,o] [i,o,o,i,o,o] @?= [i,i,o,i,i,o]-}

  {-, testCase "conv [i] [i]" $-}
    {-conv [i] [i] @?= [i]-}

  {-, testCase "conv [i] [i,o]" $-}
    {-conv [i] [i,o] @?= [i,o]-}

  {-, testCase "conv [o] [i,o]" $-}
    {-conv [o] [i,o] @?= [o,o]-}
  {-]-}

