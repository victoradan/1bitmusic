import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Music1Bit.Comp (run)
import Music1Bit.Tick (Tick(..))
import Music1Bit.Boolean ((&), (\&), (#))

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Tests" [runTest, tickTest]

{-propsTests :: TestTree-}
{-propsTests = testGroup "Properties" [runProps]-}

{-runProps = testGroup "run length"-}
  {-[-}
    {-QC.testProperty "run length" $-}
    {-\n xs -> length (run (n::Int) (xs::[Tick])) == n-}
  {-]-}

runTest = testGroup "run Phasor"
  [ testCase "run n < length" $
    run 2 [I,O,O]  @?= [I,O]

  , testCase "run n > length" $
    run 6 [I,O,O] @?= [I,O,O,I,O,O]

  , testCase "run n == 0" $
    run 0 [I,O,O] @?= []

  ]

tickTest = testGroup "a ++ b"
  [ testCase "[I,O] ++ [I]" $
    [I,O] ++ [I] @?= [I,O,I]

  , testCase "a & b" $
    ([I,I,O,O] & [I,O,I,O]) @?= [I,O,O,O]

  , testCase "a \\& b" $
    ([I,I,O,O] \& [I,O,I,O]) @?= [I,I,I,O]

  , testCase "a # b" $
    ([I,I,O,O] # [I,O,I,O]) @?= [O,I,I,O]
  ]


