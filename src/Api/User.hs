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
import           Coords                      (Coords, fromCoords)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  as LBS
import           Data.Int                    (Int64)
import           Data.Time                   (getCurrentTime)
import           Database.Esqueleto          (InnerJoin (InnerJoin), from, on,
                                              select, val, where_, (==.), (^.))
import qualified Database.Persist.Postgresql as Sql
import           GHC.Generics                (Generic)
import           Models                      (EntityField (OfferUserId, UserId, UserUsername),
                                              Offer (Offer), User (User),
                                              UserId, runDb, runSafeDb,
                                              userUsername)
import           Servant

data UserRequest = UserRequest
    { username             :: String
    , password             :: String
    , confirmationPassword :: String
    , coords               :: Coords
    , cloudinaryId         :: Maybe String
    } deriving (Show, Generic)

instance FromJSON UserRequest

type UserAPI
     = "users" :> AuthProtect "jwt-auth" :> Get '[JSON] [Sql.Entity User]
     :<|> "users" :> ReqBody '[JSON] UserRequest :> Post '[JSON] Int64
     :<|> "offers" :> AuthProtect "jwt-auth" :> Get '[JSON] [Sql.Entity Offer]

userServer :: ServerT UserAPI App
userServer = allUsersAuth :<|> createUser :<|> authUserOffers

authUserOffers :: User -> App [Sql.Entity Offer]
authUserOffers user =
    runDb $
      select $
      from $ \(offers `InnerJoin` users) -> do
      on (users ^. UserId ==. offers ^. OfferUserId)
      where_ (users ^. UserUsername ==. val (userUsername user))
      return offers

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
                            (User (username userReq) (BS.unpack pass) (cloudinaryId userReq) (fromCoords $ coords userReq) time time)
                    either
                        (throwError . apiErr . ((,) E401) . sqlError)
                        (return . Sql.fromSqlKey)
                        newUser

validateUser :: UserRequest -> Either ApiErr UserRequest
validateUser (UserRequest n p cp coords cloudId) =
    UserRequest <$> pure n <*>
    (Auth.confirmPassword p cp *> Auth.validatePasswordLength p *> pure p) <*>
      pure cp <*> pure coords <*> pure cloudId
