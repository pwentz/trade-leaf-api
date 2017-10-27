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
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  as LBS
import           Data.Int                    (Int64)
import           Data.Time                   (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectList)
import           Debug.Trace                 (trace)
import           GHC.Generics                (Generic)
import           Models                      (User (User), runDb, runSafeDb)
import           Servant

data UserRequest = UserRequest
    { username             :: String
    , email                :: String
    , password             :: String
    , confirmationPassword :: String
    } deriving (Show, Generic)

instance FromJSON UserRequest

type UserAPI
     = "users" :> AuthProtect "jwt-auth" :> Get '[ JSON] [Entity User]
     :<|> "users" :> ReqBody '[ JSON] UserRequest :> Post '[ JSON] Int64

userServer :: ServerT UserAPI App
userServer = allUsersAuth :<|> createUser

allUsersAuth :: User -> App [Entity User]
allUsersAuth user = do
    liftIO $ Prelude.putStr $ show user
    runDb $ selectList [] []

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
                        insert
                            (User
                                 (username userReq)
                                 time
                                 time
                                 (BS.unpack pass))
                    either
                        (throwError . apiErr . ((,) E401) . sqlError)
                        (return . fromSqlKey)
                        newUser

validateUser :: UserRequest -> Either ApiErr UserRequest
validateUser (UserRequest n e p cp) =
    UserRequest <$> pure n <*> pure e <*>
    (Auth.confirmPassword p cp *> Auth.validatePasswordLength p *> pure p) <*>
    pure cp
