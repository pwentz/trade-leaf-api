module CoordsSpec where

import           Coords
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "CoordsSpec" $ do
        it "can convert a string into coordinates" $ do
            (toCoords "-41.4533,12.3432") `shouldBe`
                (Just ((-41.4533), 12.3432))
            (toCoords "-41.4533") `shouldBe` Nothing
            (toCoords "-41.4533,12.3432,14.3424") `shouldBe` Nothing
        it "can calculate the distance between two coordinates (in miles)" $
        -- Chicago to Cleveland
            let start = (41.938159, (-87.642762))
                end = (41.503095, (-81.697728))
            in do (round $ distanceInMiles start end) `shouldBe` 308
        it "can convert coords into a String" $ do
            (fromCoords (42.423242, (-14.432))) `shouldBe` "42.423242,-14.432"
