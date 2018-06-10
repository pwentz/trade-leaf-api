{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app
  ) where

import           Api.Auth                         (AuthAPI, authHandler,
                                                   authServer)
import           Api.Category                     (CategoryAPI, categoryServer)
import           Api.Match                        (MatchAPI, matchServer)
import           Api.Message                      (MessageAPI, messageServer)
import           Api.Offer                        (OfferAPI, offerServer)
import           Api.Photo                        (PhotoAPI, photoServer)
import           Api.Request                      (RequestAPI, requestServer)
import           Api.Trade                        (TradeAPI, tradeServer)
import           Api.TradeChat                    (TradeChatAPI,
                                                   tradeChatServer)
import           Api.User
import           Config                           (App (..), Config (..))
import           Control.Category                 ((<<<), (>>>))
import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey,
                                                   insert, selectFirst,
                                                   selectList, (==.))
import           Models.User
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

appToMatchServer :: Config -> Server MatchAPI
appToMatchServer cfg = enter (convertApp cfg >>> NT Handler) matchServer

appToTradeServer :: Config -> Server TradeAPI
appToTradeServer cfg = enter (convertApp cfg >>> NT Handler) tradeServer

appToTradeChatServer :: Config -> Server TradeChatAPI
appToTradeChatServer cfg = enter (convertApp cfg >>> NT Handler) tradeChatServer

appToMessageServer :: Config -> Server MessageAPI
appToMessageServer cfg = enter (convertApp cfg >>> NT Handler) messageServer

appToOfferServer :: Config -> Server OfferAPI
appToOfferServer cfg = enter (convertApp cfg >>> NT Handler) offerServer

appToCategoryServer :: Config -> Server CategoryAPI
appToCategoryServer cfg = enter (convertApp cfg >>> NT Handler) categoryServer

appToRequestServer :: Config -> Server RequestAPI
appToRequestServer cfg = enter (convertApp cfg >>> NT Handler) requestServer

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = runReaderTNat cfg <<< NT runApp

type AppAPI
   =    UserAPI
   :<|> AuthAPI
   :<|> PhotoAPI
   :<|> MatchAPI
   :<|> TradeAPI
   :<|> TradeChatAPI
   :<|> MessageAPI
   :<|> OfferAPI
   :<|> CategoryAPI
   :<|> RequestAPI

appApi :: Proxy AppAPI
appApi = Proxy

genAuthServerContext :: Context (AuthHandler Request User ': '[])
genAuthServerContext = authHandler :. EmptyContext

app :: Config -> Application
app cfg =
  serveWithContext
    appApi
    genAuthServerContext
    (appToServer cfg :<|>
     appToAuthServer cfg :<|>
     appToPhotoServer cfg :<|>
     appToMatchServer cfg :<|>
     appToTradeServer cfg :<|>
     appToTradeChatServer cfg :<|>
     appToMessageServer cfg :<|>
     appToOfferServer cfg :<|>
     appToCategoryServer cfg :<|>
     appToRequestServer cfg)
