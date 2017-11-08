{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import qualified Api.Auth                    as Auth
import           Api.Error                   (ApiErr (..), StatusCode (..),
                                              apiErr, sqlError)
import           Config                      (App (..), Config (..), getConfig)
import           Control.Applicative         (liftA2)
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
import           Models                      (EntityField (UserCoordinates),
                                              Offer (Offer), User (User), runDb,
                                              runSafeDb, userUsername)
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

type UserAPI
    = "users" :> Capture "id" Int64 :> AuthProtect "jwt-auth" :> Get '[JSON] (Entity User)
     :<|> "users" :> ReqBody '[JSON] UserRequest :> Post '[JSON] Int64
     :<|> "users" :> Capture "id" Int64 :> "coordinates" :> ReqBody '[JSON] UserLocation :> AuthProtect "jwt-auth" :> Put '[JSON] ()

userServer :: ServerT UserAPI App
userServer = userInfo :<|> createUser :<|> updateCoords

getUser :: Int64 -> App (Maybe User)
getUser = runDb . get . toSqlKey

userInfo :: Int64 -> User -> App (Entity User)
userInfo userId user = do
  mbUser <- getUser userId
  if fromMaybe False (((== user)) <$> mbUser)
     then
      return (Entity (toSqlKey userId) user)
     else
      throwError $ apiErr (E401, RequestedUserNotAuth)

createUser :: UserRequest -> App Int64
createUser userReq@(UserRequest uname upass upassCon loc photoKey) =
    case validateUser userReq of
        Left e -> throwError $ apiErr (E400, e)
        Right usr -> do
            time <- liftIO getCurrentTime
            pw <- liftIO (Auth.encodePassword upass)
            case pw of
                Nothing ->
                    throwError $
                    apiErr
                        ( E401
                        , (CustomError
                               "Password was invalid. Please try another password."))
                Just pass -> do
                    newUser <-
                        runSafeDb $
                        insert
                            (User uname (BS.unpack pass) (toSqlKey <$> photoKey) (coordsFromLocation <$> loc) time time)
                    either
                        (throwError . apiErr . ((,) E401) . sqlError)
                        (return . fromSqlKey)
                        newUser

updateCoords :: Int64 -> UserLocation -> User -> App ()
updateCoords userId loc user = do
  requestedUser <- getUser userId
  if fromMaybe False ((== user) <$> requestedUser)
     then
      runDb $ update (toSqlKey userId) [UserCoordinates =. (Just $ coordsFromLocation loc)]
     else
      throwError $ apiErr (E401, RequestedUserNotAuth)

validateUser :: UserRequest -> Either ApiErr UserRequest
validateUser (UserRequest u p pc location photoId) =
    UserRequest <$> pure u <*>
    (Auth.confirmPassword p pc *> Auth.validatePasswordLength p *> pure p) <*>
      pure pc <*> pure location <*> pure photoId

coordsFromLocation :: UserLocation -> Coords
coordsFromLocation = liftA2 toCoords lat lng
