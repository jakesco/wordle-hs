module PuzzleSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Puzzle" $ do
    it "Character padding" $ do
      id 'a' `shouldBe` 'a'
