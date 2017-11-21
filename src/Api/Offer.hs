{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Offer where

import           Api.Photo                   (PhotoRequest (..))
import           Config                      (App)
import           Control.Applicative         (liftA2, liftA3)
import           Control.Monad               (join)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import qualified Database.Esqueleto          as E
import           Database.Persist.Postgresql (Entity, entityKey, entityVal,
                                              fromSqlKey, get, selectFirst,
                                              toSqlKey, (==.))
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Queries.Offer               (getOfferData, userOffers)
import           Utils                       (first, sHead)

data RequestResponse = RequestResponse
    { id          :: Int64
    , offerId     :: Int64
    , category    :: String
    , description :: String
    } deriving (Eq, Show)

data OfferResponse = OfferResponse
    { id          :: Int64
    , userId      :: Int64
    , description :: String
    , category    :: String
    , request     :: RequestResponse
    , photo       :: PhotoRequest
    } deriving (Eq, Show)

getOffers :: Int64 -> App [OfferResponse]
getOffers userId = do
    offers <- userOffers (toSqlKey userId)
    offerResponses <- sequence <$> traverse toOfferResponse offers
    return (fromMaybe [] offerResponses)

toRequestResponse :: Entity Request -> App (Maybe RequestResponse)
toRequestResponse req = do
    reqCat <-
        (categoryName <$>) <$>
        ((Db.run . get . requestCategoryId . entityVal) req)
    case reqCat of
        Nothing -> return Nothing
        Just catNm ->
            (return . return) $
            RequestResponse
                (fromSqlKey $ entityKey req)
                ((fromSqlKey . requestOfferId . entityVal) req)
                catNm
                (requestDescription $ entityVal req)

toOfferResponse :: Entity Offer -> App (Maybe OfferResponse)
toOfferResponse offer =
    let mkPhotoRes = liftA2 PhotoRequest photoCloudinaryId photoImageUrl
        mkOfferRes (_, catNm, photo) reqRes =
            OfferResponse
                (fromSqlKey $ entityKey offer)
                ((fromSqlKey . offerUserId . entityVal) offer)
                (offerDescription $ entityVal offer)
                (E.unValue catNm)
                reqRes
                (mkPhotoRes $ entityVal photo)
    in do mbData <- sHead <$> getOfferData offer
          reqRes <- join <$> traverse toRequestResponse (first <$> mbData)
          return (liftA2 mkOfferRes mbData reqRes)
