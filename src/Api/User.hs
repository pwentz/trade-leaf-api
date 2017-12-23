{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Api.User where

import qualified Api.Auth                    as Auth
import           Api.Error                   (ApiErr (..), StatusCode (..),
                                              apiErr, sqlError)
import           Api.Offer                   (OfferResponse, getOffers)
import           Config                      (App (..), Config (..), getConfig)
import           Control.Applicative         (liftA2)
import           Control.Monad               (join)
import qualified Control.Monad.Except        as BE
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  as LBS
import           Data.Coords                 (Coords (Coords))
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), Key, fromSqlKey, get,
                                              insert, toSqlKey, update, (=.))
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Offer
import           Models.Photo
import           Models.User
import           Servant

{-|
      -- PHOTO --
  {
    "id": Int64,
    "cloudinaryId": Maybe String,
    "imageUrl": String
  }
      -- REQUEST --
  {
      "id": Int64,
      "offerId": Int64,
      "category": String,
      "description": String
  }

      -- OFFER --
  {
      "id": Int64,
      "userId": Int64,
      "description": String,
      "category": String,
      "request": REQUEST,
      "photo": PHOTO
  }

      -- USER --
  {
      "id": Int64,
      "firstName": String,
      "lastName": String,
      "email": String,
      "username": String,
      "photo": PHOTO,
      "coordinates": { "lat": Double, "lng": Double },
      "offers": [OFFER]
  }
-}

data UserRequest = UserRequest
    { firstName   :: String
    , lastName    :: String
    , email       :: String
    , username    :: String
    , password    :: String
    , photoId     :: Maybe Int64
    , coordinates :: Maybe Coords
    } deriving (Show, Generic)

instance FromJSON UserRequest

data UserPatchRequest = UserPatchRequest
    { firstName   :: Maybe String
    , lastName    :: Maybe String
    , email       :: Maybe String
    , username    :: Maybe String
    , photoId     :: Maybe Int64
    , coordinates :: Maybe Coords
    } deriving (Show, Generic)

instance FromJSON UserPatchRequest

data UserMeta = UserMeta
    { id          :: Int64
    , firstName   :: String
    , lastName    :: String
    , email       :: String
    , username    :: String
    , photo       :: Maybe (Entity Photo)
    , coordinates :: Maybe Coords
    , offers      :: [OfferResponse]
    } deriving (Eq, Show, Generic)

instance ToJSON UserMeta

type UserAPI
     = "users" :> Capture "id" Int64 :> Get '[JSON] UserMeta
      :<|> "users" :> ReqBody '[JSON] UserRequest :> Post '[JSON] Int64
      :<|> "users" :> Capture "id" Int64 :> ReqBody '[JSON] UserPatchRequest :> AuthProtect "jwt-auth" :> Patch '[JSON] ()

userServer :: ServerT UserAPI App
userServer = getUserMeta :<|> createUser :<|> patchUser

getUser :: Int64 -> App (Maybe User)
getUser = Db.run . get . toSqlKey

createUser :: UserRequest -> App Int64
createUser userReq@UserRequest {..} =
    case validateUser userReq of
        Left e -> throwError $ apiErr (E400, e)
        Right usr -> do
            pw <- liftIO (Auth.encodePassword password)
            case pw of
                Nothing ->
                    throwError $
                    apiErr
                        (E401, CustomError "Password was invalid. Please try another password.")
                Just pass -> do
                    time <- liftIO getCurrentTime
                    newUser <-
                        Db.runSafe $
                        insert
                            User
                              { userFirstName = firstName
                              , userLastName = lastName
                              , userEmail = email
                              , userUsername = username
                              , userPassword = BS.unpack pass
                              , userPhotoId = toSqlKey <$> photoId
                              , userCoordinates = coordinates
                              , userCreatedAt = time
                              , userUpdatedAt = time
                              }
                    either
                        (throwError . apiErr . (,) E401 . sqlError)
                        (return . fromSqlKey)
                        newUser

patchUser :: Int64 -> UserPatchRequest -> User -> App ()
patchUser userId UserPatchRequest {..} user = do
    requestedUser <- getUser userId
    if fromMaybe False ((== user) <$> requestedUser)
        then do
            traverse updateFirstName firstName
            traverse updateLastName lastName
            traverse updateEmail email
            traverse updateUsername username
            traverse updatePhotoId photoId
            traverse updateCoords coordinates
            return ()
        else throwError $ apiErr (E401, RequestedUserNotAuth)
  where
    updateFirstName name =
        Db.run $ update (toSqlKey userId) [UserFirstName =. name]
    updateLastName name =
        Db.run $ update (toSqlKey userId) [UserLastName =. name]
    updateEmail email =
        Db.run $ update (toSqlKey userId) [UserEmail =. email]
    updateUsername name =
        Db.run $ update (toSqlKey userId) [UserUsername =. name]
    updatePhotoId pId =
        Db.run $ update (toSqlKey userId) [UserPhotoId =. Just (toSqlKey pId)]
    updateCoords coords =
        Db.run $ update (toSqlKey userId) [UserCoordinates =. Just coords]

getUserMeta :: Int64 -> App UserMeta
getUserMeta userId = do
    mbUser <- getUser userId
    mbPhoto <- join <$> traverse (Db.run . get) (userPhotoId =<< mbUser)
    offers <- getOffers userId
    case mbUser of
        Nothing -> throwError $ apiErr (E404, UserNotFound userId)
        Just User {..} ->
          return
            UserMeta
              { id = userId
              , firstName = userFirstName
              , lastName = userLastName
              , email = userEmail
              , username = userUsername
              , photo = liftA2 Entity userPhotoId mbPhoto
              , coordinates = userCoordinates
              , offers = offers
              }

validateUser :: UserRequest -> Either ApiErr UserRequest
validateUser userReq@UserRequest {..} = do
  Auth.validatePasswordLength password
  return userReq
