module Data.CoordsSpec where

import Data.Coords
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "CoordsSpec" $
        it "can calculate the distance between two coordinates (in miles)" $
            let start = Coords 41.938132 (-87.642753)
            in do
              round (distanceInMiles start (Coords 41.858210 (-87.651700))) `shouldBe` 6
              round (distanceInMiles start (Coords 41.888730 (-87.687969))) `shouldBe` 4
              round (distanceInMiles start (Coords 41.680753 (-87.698157))) `shouldBe` 18
              round (distanceInMiles start (Coords 41.734517 (-87.674043))) `shouldBe` 14
              round (distanceInMiles start (Coords 41.804575 (-87.671359))) `shouldBe` 9
