module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Music1Bit.Combinators


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "run" $ do
    it "run returns the correct number of values" $ do
      run (silence 1) `shouldBe` [False]
      run (silence 0) `shouldBe` []
      run (silence (-1)) `shouldBe` []
