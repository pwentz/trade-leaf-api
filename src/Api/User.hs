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
import qualified Control.Monad.Except        as BE
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import           Data.Coords                 (Coords, fromCoords)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  as LBS
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Sql
import           GHC.Generics                (Generic)
import           MatchFinder                 (findMatches)
import           Models                      (EntityField (OfferUserId, UserId, UserUsername),
                                              Offer (Offer), User (User),
                                              UserId, runDb, runSafeDb,
                                              userUsername)
import           Servant

data UserRequest = UserRequest
    { username             :: String
    , password             :: String
    , passwordConfirmation :: String
    , location             :: UserLocation
    } deriving (Show, Generic)

instance FromJSON UserRequest

data UserLocation = UserLocation
  { lat :: Double
  , lng :: Double
  } deriving (Show, Generic)

instance FromJSON UserLocation

type UserAPI
    = "users" :> Capture "id" Int64 :> AuthProtect "jwt-auth" :> Get '[JSON] (Sql.Entity User)
     :<|> "users" :> ReqBody '[JSON] UserRequest :> Post '[JSON] Int64
     :<|> "offers" :> AuthProtect "jwt-auth" :> Get '[JSON] [Sql.Entity Offer]

userServer :: ServerT UserAPI App
userServer = userInfo :<|> createUser :<|> findMatches

userInfo :: Int64 -> User -> App (Sql.Entity User)
userInfo userId user = do
  mbUser <- runDb $ Sql.get (Sql.toSqlKey userId :: Sql.Key User)
  if fromMaybe False (((== user)) <$> mbUser)
     then
      return (Sql.Entity (Sql.toSqlKey userId) user)
     else
      throwError $
        apiErr (E401, CustomError "User requested does not match user associated with provided auth token.")

allUsersAuth :: User -> App [Sql.Entity User]
allUsersAuth user = do
    liftIO $ Prelude.putStr $ show user
    runDb $ Sql.selectList [] []

createUser :: UserRequest -> App Int64
createUser userReq =
    case validateUser userReq of
        Left e -> throwError $ apiErr (E400, e)
        Right usr -> do
            time <- liftIO getCurrentTime
            pw <- liftIO (Auth.encodePassword (password userReq))
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
                        Sql.insert
                            (User (username userReq) (BS.unpack pass) Nothing (fromLocation $ location userReq) time time)
                    either
                        (throwError . apiErr . ((,) E401) . sqlError)
                        (return . Sql.fromSqlKey)
                        newUser
                          where
                            fromLocation (UserLocation lat lng) =
                              fromCoords (lat, lng)

validateUser :: UserRequest -> Either ApiErr UserRequest
validateUser (UserRequest u p pc location) =
    UserRequest <$> pure u <*>
    (Auth.confirmPassword p pc *> Auth.validatePasswordLength p *> pure p) <*>
      pure pc <*> pure location
