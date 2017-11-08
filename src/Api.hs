{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api
    ( app
    ) where

import           Api.Auth                         (AuthAPI, authHandler,
                                                   authServer)
import           Api.User
import           Api.Photo (PhotoAPI, photoServer)
import           Config                           (App (..), Config (..))
import           Control.Category                 ((<<<), (>>>))
import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey,
                                                   insert, selectFirst,
                                                   selectList, (==.))
import           Models                           (User)
import           Network.Wai                      (Application, Request)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)

appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg >>> NT Handler) userServer

appToAuthServer :: Config -> Server AuthAPI
appToAuthServer cfg = enter (convertApp cfg >>> NT Handler) authServer

appToPhotoServer :: Config -> Server PhotoAPI
appToPhotoServer cfg = enter (convertApp cfg >>> NT Handler) photoServer

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = runReaderTNat cfg <<< NT runApp

type AppAPI = UserAPI :<|> AuthAPI :<|> PhotoAPI

appApi :: Proxy AppAPI
appApi = Proxy

genAuthServerContext :: Context (AuthHandler Request User ': '[])
genAuthServerContext = authHandler :. EmptyContext

app :: Config -> Application
app cfg =
    serveWithContext
        appApi
        genAuthServerContext
        (appToServer cfg :<|> appToAuthServer cfg :<|> appToPhotoServer cfg)
