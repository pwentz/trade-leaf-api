module UtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Utils

spec :: Spec
spec =
    describe "UtilsSpec" $
        it "can retrieve the head of a list" $ do
            sHead [1 .. 3] `shouldBe` Just 1
            sHead ([] :: String) `shouldBe` Nothing
