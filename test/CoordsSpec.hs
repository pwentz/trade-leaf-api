module CoordsSpec where

import Coords
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "CoordsSpec" $ do
        it "can convert a string into coordinates" $ do
            (toCoords "-41.4533,12.3432") `shouldBe`
                (Just ((-41.4533), 12.3432))
            (toCoords "-41.4533") `shouldBe` Nothing
            (toCoords "-41.4533,12.3432,14.3424") `shouldBe` Nothing
        it "can calculate the distance between two coordinates (in miles)" $
            let start = (41.938132, -87.642753)
            in do
              (round $ distanceInMiles start (41.858210, -87.651700)) `shouldBe` 6
              (round $ distanceInMiles start (41.888730, -87.687969)) `shouldBe` 4
              (round $ distanceInMiles start (41.680753, -87.698157)) `shouldBe` 18
              (round $ distanceInMiles start (41.734517, -87.674043)) `shouldBe` 14
              (round $ distanceInMiles start (41.804575, -87.671359)) `shouldBe` 9
        it "can convert coords into a String" $ do
            (fromCoords (42.423242, (-14.432))) `shouldBe` "42.423242,-14.432"
