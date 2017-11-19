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
import           Database.Persist.Postgresql (Entity, entityKey, entityVal,
                                              fromSqlKey, get, selectFirst,
                                              toSqlKey, (==.))
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Queries.Offer               (userOffers)

data RequestResponse = RequestResponse
    { offerId     :: Int64
    , category    :: String
    , description :: String
    } deriving (Eq, Show)

data OfferResponse = OfferResponse
    { userId      :: Int64
    , description :: String
    , category    :: String
    , request     :: RequestResponse
    , photo       :: PhotoRequest
    } deriving (Eq, Show)

getOffers :: Int64 -> App [OfferResponse]
getOffers userId = do
  offers <- userOffers (toSqlKey userId)
  offerResponses <- sequence <$> traverse toOfferResponse offers
  return $ fromMaybe [] offerResponses

toOfferResponse :: Entity Offer -> App (Maybe OfferResponse)
toOfferResponse offer =
  let
    toPhotoReq =
      liftA2 PhotoRequest photoCloudinaryId photoImageUrl
    toReq req catNm =
      RequestResponse (fromSqlKey $ requestOfferId req) catNm (requestDescription req)
  in do
  mbReq <- (entityVal <$>) <$> (Db.run $ selectFirst [RequestOfferId ==. (entityKey offer)] [])
  mbReqCatNm <- ((categoryName <$>) . join) <$> traverse (Db.run . get . requestCategoryId) mbReq
  mbPhoto <- Db.run $ get (offerPhotoId $ entityVal offer)
  mbCategoryNm <- (categoryName <$>) <$> (Db.run $ get (offerCategoryId $ entityVal offer))
  return $ do
    reqRes <- liftA2 toReq mbReq mbReqCatNm
    photoData <- toPhotoReq <$> mbPhoto
    catNm <- mbCategoryNm
    return $
      OfferResponse
        { userId = (fromSqlKey . offerUserId . entityVal) offer
        , description = offerDescription $ entityVal offer
        , category = catNm
        , request = reqRes
        , photo = photoData
        }
