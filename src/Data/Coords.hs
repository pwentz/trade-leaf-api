{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Coords
    ( distanceInMiles
    , Coords(Coords)
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

data Coords =
    Coords
      { lat :: Double
      , lng :: Double
      }
    deriving (Eq, Show, Generic)

instance ToJSON Coords

instance FromJSON Coords

instance Sql.PersistField Coords where
    toPersistValue coords = Sql.PersistDbSpecific (BS.pack $ fromCoords coords)
    fromPersistValue (Sql.PersistDbSpecific t) =
        Right $ toCoords (BS.unpack t)
    fromPersistValue _ = Left "Coords must be converted from PersistDbSpecific"

instance Sql.PersistFieldSql Coords where
    sqlType _ = Sql.SqlOther "POINT"

distanceInMiles :: Coords -> Coords -> Double
distanceInMiles (Coords startLat startLng) (Coords endLat endLng) =
    toMiles $ distance (mkPnt startLat startLng) (mkPnt endLat endLng)
  where
    mkPnt x y = pt x y Nothing Nothing
    toMiles = (/ 1609.34)

toCoords :: String -> Coords
toCoords coords =
    let (lat:(lng:_)) = splitCoords $ dropParens coords
    in Coords lat lng
  where
    dropParens = drop 1 . init
    splitCoords = fmap read . split ","

fromCoords :: Coords -> String
fromCoords (Coords lat lng) = concat [show lat, ",", show lng]
