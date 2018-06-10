{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Request where

import qualified Api.Error                   as Err
import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromJust)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Category
import           Models.Offer
import           Models.Request
import           Models.User
import qualified Queries.User                as UserQuery
import           Servant

data RequestResponse = RequestResponse
    { id          :: Int64
    , offerId     :: Int64
    , category    :: String
    , description :: String
    } deriving (Eq, Show, Generic)

data RequestRequest = RequestRequest
    { offerId     :: Int64
    , categoryId  :: Int64
    , description :: String
    } deriving (Eq, Show, Generic)

instance FromJSON RequestRequest
instance ToJSON RequestResponse

type RequestAPI
    = "requests" :> ReqBody '[JSON] RequestRequest :> AuthProtect "jwt-auth" :> Post '[JSON] Int64

-- requestServer :: ServerT RequestAPI App
requestServer = createRequest

toRequestResponse :: Pg.Entity Request -> App (Maybe RequestResponse)
toRequestResponse (Pg.Entity reqKey reqVal) = do
    reqCat <- (categoryName <$>) <$> (Db.run . Pg.get . requestCategoryId) reqVal
    case reqCat of
        Nothing -> return Nothing
        Just catNm ->
            (return . return)
                RequestResponse
                { Api.Request.id = Pg.fromSqlKey reqKey
                , offerId = Pg.fromSqlKey (requestOfferId reqVal)
                , category = catNm
                , description = requestDescription reqVal
                }

createRequest :: RequestRequest -> User -> App Int64
createRequest RequestRequest{..} User{..} = do
  currentUserKey <- (fmap . fmap) Pg.entityKey (UserQuery.findByUsername userUsername)
  mbInvolvedOffer <- Db.run (Pg.get $ Pg.toSqlKey offerId) :: App (Maybe Offer)
  if maybe False ((== (fromJust currentUserKey)) . offerUserId) mbInvolvedOffer
     then do
       time <- liftIO getCurrentTime
       eitherReq <-
         Db.runSafe $
           Pg.insert
             Request
               { requestOfferId = Pg.toSqlKey offerId
               , requestCategoryId = Pg.toSqlKey categoryId
               , requestDescription = description
               , requestCreatedAt = time
               , requestUpdatedAt = time
               }
       either
        (throwError . Err.apiErr . (,) Err.E400 . Err.sqlError)
        (return . Pg.fromSqlKey)
        eitherReq
     else throwError $ Err.apiErr (Err.E401, Err.Unauthorized)
