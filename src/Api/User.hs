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
    , coordinates          :: Maybe Coords
    , photoId              :: Maybe Int64
    } deriving (Show, Generic)

instance FromJSON UserRequest

data UserPatchRequest = UserPatchRequest
  { username    :: Maybe String
  , photoId     :: Maybe Int64
  , coordinates :: Maybe Coords
  } deriving (Show, Generic)

instance FromJSON UserPatchRequest

data UserMeta = UserMeta
  { id          :: Int64
  , username    :: String
  , photo       :: Maybe Photo
  , coordinates :: Maybe Coords
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
                            (User username (BS.unpack pass) (toSqlKey <$> photoId) coordinates time time)
                    either
                        (throwError . apiErr . ((,) E401) . sqlError)
                        (return . fromSqlKey)
                        newUser

patchUser :: Int64 -> UserPatchRequest -> User -> App ()
patchUser userId UserPatchRequest{..} user = do
  requestedUser <- getUser userId
  if fromMaybe False ((== user) <$> requestedUser)
      then do
        traverse updateUsername username
        traverse updatePhotoId photoId
        traverse updateCoords coordinates
        return ()
      else
        throwError $ apiErr (E401, RequestedUserNotAuth)
  where
      updateUsername name =
        runDb $ update ((toSqlKey userId) :: Key User) [UserUsername =. name]
      updatePhotoId pId =
        runDb $ update ((toSqlKey userId) :: Key User) [UserPhotoId =. (Just (toSqlKey pId))]
      updateCoords coords =
        runDb $ update ((toSqlKey userId) :: Key User) [UserCoordinates =. (Just coords)]

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
