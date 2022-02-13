module WordleSpec (spec) where

import Test.Hspec
import Test.QuickCheck

tmpPad :: Char -> String
tmpPad c = [' ', c, ' ']

spec :: Spec
spec = do
  describe "Test the testing" $ do
    it "Temp Pad" $ do
      property $ \x -> x + 1 > (x :: Int)
