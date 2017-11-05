module CoordsSpec where

import Data.Coords
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "CoordsSpec" $ do
        it "can convert two doubles into coords" $ do
            getCoords (toCoords 12.345 (-343.34)) `shouldBe` (12.345,(-343.34))
        -- it "can convert a string into coordinates" $ do
        --     (toCoords "-41.4533,12.3432") `shouldBe`
        --         (Just ((-41.4533), 12.3432))
        --     (toCoords "-41.4533") `shouldBe` Nothing
        --     (toCoords "-41.4533,12.3432,14.3424") `shouldBe` Nothing
        it "can calculate the distance between two coordinates (in miles)" $
            let start = toCoords 41.938132 (-87.642753)
            in do
              (round $ distanceInMiles start (toCoords 41.858210 (-87.651700))) `shouldBe` 6
              (round $ distanceInMiles start (toCoords 41.888730 (-87.687969))) `shouldBe` 4
              (round $ distanceInMiles start (toCoords 41.680753 (-87.698157))) `shouldBe` 18
              (round $ distanceInMiles start (toCoords 41.734517 (-87.674043))) `shouldBe` 14
              (round $ distanceInMiles start (toCoords 41.804575 (-87.671359))) `shouldBe` 9
