module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Music1Bit.Combinators


main :: IO ()
main = hspec $ do
  describe "run" $ do
    it "run returns the correct number of values" $ do
      run (silence 1) `shouldBe` [False]
      run (silence 0) `shouldBe` []
      run (silence (-1)) `shouldBe` []
