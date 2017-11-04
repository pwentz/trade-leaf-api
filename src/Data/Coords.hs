{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Coords
    ( toCoords
    , distanceInMiles
    , fromCoords
    , Coords
    , Geo
    ) where

import           Control.Applicative   (liftA2)
import qualified Data.ByteString.Char8 as BS
import           Data.String.Utils     (split)
import qualified Database.Persist.Sql  as Sql
import           Geo.Computations      (Point, distance, pt)
import           Text.Read             (readMaybe)
import GHC.Generics (Generic)

data Geo =
    Geo BS.ByteString
    deriving (Eq, Show, Generic)

instance Sql.PersistField Geo where
    toPersistValue (Geo t) = Sql.PersistDbSpecific t
    fromPersistValue (Sql.PersistDbSpecific t) =
        Right $ Geo $ BS.concat ["'", t, "'"]
    fromPersistValue _ =
        Left "Geo values must be converted from PersistDbSpecific"

instance Sql.PersistFieldSql Geo where
    sqlType _ = Sql.SqlOther "GEOGRAPHY(POINT,4326)"

toPoint :: Double -> Double -> Geo
toPoint lat lon = Geo $ BS.concat ["'POINT(", ps $ lon, " ", ps $ lat, ")'"]
  where
    ps = BS.pack . show

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
