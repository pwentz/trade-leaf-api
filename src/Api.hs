{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api
    ( app
    ) where

import           Api.Auth                         (AuthAPI, authHandler,
                                                   authServer)
import           Api.User
import           Config                           (App (..), Config (..))
import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey,
                                                   insert, selectFirst,
                                                   selectList, (==.))
import           Models
import           Network.Wai                      (Application, Request)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)

appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg) userServer

appToAuthServer :: Config -> Server AuthAPI
appToAuthServer cfg = enter (convertApp cfg) authServer

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

type AppAPI = UserAPI :<|> AuthAPI

appApi :: Proxy AppAPI
appApi = Proxy

genAuthServerContext :: Context (AuthHandler Request User ': '[])
genAuthServerContext = authHandler :. EmptyContext

app :: Config -> Application
app cfg =
    serveWithContext
        appApi
        genAuthServerContext
        (appToServer cfg :<|> appToAuthServer cfg)
