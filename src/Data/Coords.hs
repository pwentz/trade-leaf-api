module Data.Coords
    ( toCoords
    , distanceInMiles
    , fromCoords
    , Coords
    ) where

import           Control.Applicative (liftA2)
import           Data.String.Utils   (split)
import           Geo.Computations    (Point, distance, pt)
import           Text.Read           (readMaybe)

type Coords = (Double, Double)

toCoords :: String -> Maybe Coords
toCoords str
    | length splitCoords == 2 =
        toCoords' (head splitCoords) (head $ tail splitCoords)
    | otherwise = Nothing
  where
    splitCoords = split "," str
    toCoords' lat lng = liftA2 (,) (readMaybe lat) (readMaybe lng)

distanceInMiles :: Coords -> Coords -> Double
distanceInMiles start end = toMiles $ distance (mkPnt start) (mkPnt end)
  where
    mkPnt (lat, lng) = pt lat lng Nothing Nothing
    toMiles = (/ 1609.34)

fromCoords :: Coords -> String
fromCoords (lat, lng) = concat [show lat, ",", show lng]
