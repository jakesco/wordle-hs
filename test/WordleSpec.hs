module WordleSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Test the testing" $ do
    it "Does some testing" $ do
      1 + 1 `shouldBe` 2
