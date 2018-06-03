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

instance ToJSON OfferResponse

type OfferAPI
    = "offers" :> Capture "id" Int64 :> AuthProtect "jwt-auth" :> Delete '[JSON] ()

-- offerServer :: ServerT OfferAPI App
offerServer = removeOffer

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
