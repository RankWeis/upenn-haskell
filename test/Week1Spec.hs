module Week1Spec (spec) where

import Week1
import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec

spec = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
  describe "Something" $ do
    it "returns a reversed list" $ do
        reverseList [1..5] `shouldBe` [5,4,3,2,1]

