{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Offer where

import qualified Api.Error                   as Err
import           Api.Request                 (RequestResponse,
                                              toRequestResponse)
import           Config                      (App)
import           Control.Applicative         (liftA2)
import           Control.Monad               (join)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Database.Esqueleto          as E
import           Database.Persist.Postgresql
import           Data.Time                   (getCurrentTime)
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.User
import           Queries.Offer               (destroyOffer, getOfferData,
                                              userOffers)
import           Queries.Request             (getOfferRequest)
import qualified Queries.User                as UserQuery
import           Servant
import           Utils                       (first, sHead)

data OfferResponse = OfferResponse
    { id          :: Int64
    , userId      :: Int64
    , description :: String
    , category    :: String
    , request     :: Maybe RequestResponse
    , photo       :: Entity Photo
    } deriving (Eq, Show, Generic)

data OfferRequest = OfferRequest
    { categoryId  :: Int64
    , photoId     :: Int64
    , description :: String
    , radius      :: Double
    } deriving (Eq, Show, Generic)

instance ToJSON OfferResponse
instance FromJSON OfferRequest

type OfferAPI
    = "offers" :> Capture "id" Int64 :> AuthProtect "jwt-auth" :> Delete '[JSON] ()
      :<|> "offers" :> ReqBody '[JSON] OfferRequest :> AuthProtect "jwt-auth" :> Post '[JSON] Int64

-- offerServer :: ServerT OfferAPI App
offerServer = removeOffer :<|> createOffer

createOffer :: OfferRequest -> User -> App Int64
createOffer OfferRequest{..} User {..} = do
  time <- liftIO getCurrentTime
  mbCurrentUserKey <- (fmap . fmap) entityKey (UserQuery.findByUsername userUsername)
  eitherOffer <-
    Db.runSafe $
      insert
        Offer
          { offerUserId = fromJust mbCurrentUserKey
          , offerCategoryId = toSqlKey categoryId
          , offerPhotoId = toSqlKey photoId
          , offerDescription = description
          , offerRadius = radius
          , offerCreatedAt = time
          , offerUpdatedAt = time
          }
  case eitherOffer of
    Left err -> throwError $ Err.apiErr (Err.E400, Err.sqlError err)
    Right offerKey ->
      return (fromSqlKey offerKey)


removeOffer :: Int64 -> User -> App ()
removeOffer offerId User{..} = do
  mbCurrentUserKey <- (fmap . fmap) entityKey (UserQuery.findByUsername userUsername)
  mbOffer <- Db.run (get $ toSqlKey offerId) :: App (Maybe Offer)
  if fromMaybe False $ liftA2 ((==) . offerUserId) mbOffer mbCurrentUserKey
     then destroyOffer (toSqlKey offerId)
     else throwError $ Err.apiErr (Err.E401, Err.Unauthorized)


getOffers :: Int64 -> App [OfferResponse]
getOffers userId = traverse toOfferResponse =<< userOffers (toSqlKey userId)

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
    in liftA2
           mkOfferRes
           (join <$> (traverse toRequestResponse =<< getOfferRequest offerKey))
           (getOfferData offer)
