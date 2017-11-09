{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import qualified Api.Auth                    as Auth
import           Api.Error                   (ApiErr (..), StatusCode (..),
                                              apiErr, sqlError)
import           Config                      (App (..), Config (..), getConfig)
import           Control.Applicative         (liftA2)
import           Control.Monad               (join)
import qualified Control.Monad.Except        as BE
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  as LBS
import           Data.Coords                 (Coords, toCoords)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), Key, fromSqlKey, get,
                                              insert, toSqlKey, update, (=.))
import           GHC.Generics                (Generic)
import           Models                      (EntityField (UserCoordinates, UserPhotoId, UserUsername),
                                              Photo (Photo), User (User), runDb,
                                              runSafeDb, userCoordinates,
                                              userPhotoId, userUsername)
import           Servant

data UserRequest = UserRequest
    { username             :: String
    , password             :: String
    , passwordConfirmation :: String
    , location             :: Maybe UserLocation
    , photoId              :: Maybe Int64
    } deriving (Show, Generic)

instance FromJSON UserRequest

data UserLocation = UserLocation
    { lat :: Double
    , lng :: Double
    } deriving (Show, Generic)

instance FromJSON UserLocation

data UserPatchRequest = UserPatchRequest
  { patchUsername    :: Maybe String
  , patchPhotoId     :: Maybe Int64
  , patchCoordinates :: Maybe UserLocation
  } deriving (Show, Generic)

instance FromJSON UserPatchRequest

data UserMeta = UserMeta
  { metaId       :: Int64
  , metaUsername :: String
  , metaPhoto    :: Maybe Photo
  , metaCoords   :: Maybe Coords
  } deriving (Show, Generic)

instance ToJSON UserMeta

type UserAPI
    = "users" :> Capture "id" Int64 :> Get '[JSON] UserMeta
     :<|> "users" :> ReqBody '[JSON] UserRequest :> Post '[JSON] Int64
     :<|> "users" :> Capture "id" Int64 :> ReqBody '[JSON] UserPatchRequest :> AuthProtect "jwt-auth" :> Patch '[JSON] ()

userServer :: ServerT UserAPI App
userServer = getUserMeta :<|> createUser :<|> patchUser

getUser :: Int64 -> App (Maybe User)
getUser = runDb . get . toSqlKey

createUser :: UserRequest -> App Int64
createUser userReq@UserRequest{..} =
    case validateUser userReq of
        Left e -> throwError $ apiErr (E400, e)
        Right usr -> do
            pw <- liftIO (Auth.encodePassword password)
            case pw of
                Nothing ->
                    throwError $
                    apiErr
                        ( E401
                        , (CustomError
                               "Password was invalid. Please try another password."))
                Just pass -> do
                    time <- liftIO getCurrentTime
                    newUser <-
                        runSafeDb $
                        insert
                            (User username (BS.unpack pass) (toSqlKey <$> photoId) (coordsFromLocation <$> location) time time)
                    either
                        (throwError . apiErr . ((,) E401) . sqlError)
                        (return . fromSqlKey)
                        newUser

patchUser :: Int64 -> UserPatchRequest -> User -> App ()
patchUser userId UserPatchRequest{..} user = do
  requestedUser <- getUser userId
  if fromMaybe False ((== user) <$> requestedUser)
      then do
        traverse updateUsername patchUsername
        traverse updatePhotoId patchPhotoId
        traverse updateCoords patchCoordinates
        return ()
      else
        throwError $ apiErr (E401, RequestedUserNotAuth)
  where
      updateUsername username =
        runDb $ update ((toSqlKey userId) :: Key User) [UserUsername =. username]
      updatePhotoId pId =
        runDb $ update ((toSqlKey userId) :: Key User) [UserPhotoId =. (Just (toSqlKey pId))]
      updateCoords userLoc =
        runDb $ update ((toSqlKey userId) :: Key User) [UserCoordinates =. (Just $ coordsFromLocation userLoc)]

getUserMeta :: Int64 -> App UserMeta
getUserMeta userId = do
  mbUser <- getUser userId
  mbPhoto <- join <$> traverse (runDb . get) (userPhotoId =<< mbUser)
  case mbUser of
      Nothing ->
        throwError $ apiErr (E404, UserNotFound userId)
      Just user ->
        return (UserMeta userId (userUsername user) mbPhoto (userCoordinates user))

validateUser :: UserRequest -> Either ApiErr UserRequest
validateUser (UserRequest u p pc location photoId) =
    UserRequest <$> pure u <*>
    (Auth.confirmPassword p pc *> Auth.validatePasswordLength p *> pure p) <*>
      pure pc <*> pure location <*> pure photoId

coordsFromLocation :: UserLocation -> Coords
coordsFromLocation = liftA2 toCoords lat lng
