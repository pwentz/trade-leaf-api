{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Coords
    ( distanceInMiles
    , toCoords
    , Coords
    , getCoords
    ) where

import           Control.Applicative   (liftA2)
import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BS
import           Data.String.Utils     (split)
import qualified Database.Persist.Sql  as Sql
import           Debug.Trace           (trace)
import           Geo.Computations      (distance, pt)
import           GHC.Generics          (Generic)
import           Text.Read             (readMaybe)

type Point = (Double, Double)

data Coords = Coords
    { getCoords :: Point
    } deriving (Eq, Show, Generic)

instance ToJSON Coords
instance FromJSON Coords

instance Sql.PersistField Coords where
    toPersistValue (Coords t) = Sql.PersistDbSpecific (BS.pack $ fromPoint t)
    fromPersistValue (Sql.PersistDbSpecific t) =
        Right $ Coords (toPoint $ BS.unpack t)
    fromPersistValue _ =
        Left "Coords must be converted from PersistDbSpecific"

instance Sql.PersistFieldSql Coords where
    sqlType _ = Sql.SqlOther "POINT"

toCoords :: Double -> Double -> Coords
toCoords = curry Coords

distanceInMiles :: Coords -> Coords -> Double
distanceInMiles (Coords start) (Coords end) =
    toMiles $ distance (mkPnt start) (mkPnt end)
  where
    mkPnt (lat, lng) = pt lat lng Nothing Nothing
    toMiles = (/ 1609.34)

toPoint :: String -> Point
toPoint coords =
    let (lat:(lng:_)) = splitCoords $ dropParens coords
    in (lat, lng)
  where
    dropParens = drop 1 . init
    splitCoords = fmap read . split ","

fromPoint :: Point -> String
fromPoint (lat, lng) = concat [show lat, ",", show lng]
