{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Offer where

import           Api.Request                 (RequestResponse,
                                              toRequestResponse)
import           Config                      (App)
import           Control.Applicative         (liftA2, liftA3)
import           Control.Monad               (join)
import           Data.Aeson                  (ToJSON)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import qualified Database.Esqueleto          as E
import           Database.Persist.Postgresql
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Queries.Offer               (getOfferData, userOffers)
import           Queries.Request             (getOfferRequest)
import           Utils                       (first, sHead)

data OfferResponse = OfferResponse
    { id          :: Int64
    , userId      :: Int64
    , description :: String
    , category    :: String
    , request     :: Maybe RequestResponse
    , photo       :: Entity Photo
    } deriving (Eq, Show, Generic)

instance ToJSON OfferResponse

getOffers :: Int64 -> App [OfferResponse]
getOffers userId =
    traverse toOfferResponse =<< userOffers (toSqlKey userId)

toOfferResponse :: Entity Offer -> App OfferResponse
toOfferResponse offer@(Entity offerKey offerVal) =
    let mkOfferRes reqRes (_, catNm, photo) =
            OfferResponse
                { id = fromSqlKey offerKey
                , userId = fromSqlKey (offerUserId offerVal)
                , description = offerDescription offerVal
                , category = E.unValue catNm
                , request = reqRes
                , photo = photo
                }
    in do mbData <- getOfferData offer
          reqRes <- join <$> (traverse toRequestResponse =<< getOfferRequest offerKey)
          return (mkOfferRes reqRes mbData)
