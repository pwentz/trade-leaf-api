{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api.Message where

import qualified Api.Error as Err
import Config (App)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Time (getCurrentTime)
import qualified Database.Persist.Sql as Sql
import qualified Db.Main as Db
import GHC.Generics (Generic)
import Models.Message
import Models.User
import qualified Queries.Message as Query
import qualified Queries.TradeChat as TCQuery
import qualified Queries.User as UserQuery
import Servant
import Data.Maybe (fromJust)
import Models.TradeChat

data MessageRequest = MessageRequest
  { tradeChatId :: Int64
  , content :: String
  } deriving (Eq, Show, Generic)

instance FromJSON MessageRequest

type MessageAPI
    = "messages" :> ReqBody '[JSON] MessageRequest :> AuthProtect "jwt-auth" :> Post '[JSON] Int64
      :<|> "messages" :> Capture "id" Int64 :> AuthProtect "jwt-auth" :> Get '[JSON] [Sql.Entity Message]

-- messageServer :: ServerT MessageAPI App
messageServer = createMessage :<|> getMessages

createMessage :: MessageRequest -> User -> App Int64
createMessage MessageRequest {..} User {..} = do
  mbCurrentUserKey <- (fmap . fmap) Sql.entityKey (UserQuery.findByUsername userUsername)
  isGivenUserValid <- isInvolvedInChat tradeChatId userUsername
  if isGivenUserValid
     then do
       time <- liftIO getCurrentTime
       eitherMsg <-
         Db.runSafe $
         Sql.insert
           Message
           { messageTradeChatId = Sql.toSqlKey tradeChatId
           , messageSenderId = fromJust mbCurrentUserKey
           , messageContent = content
           , messageCreatedAt = time
           , messageUpdatedAt = time
           }
       case eitherMsg of
         Left err -> throwError $ Err.apiErr (Err.E400, Err.sqlError err)
         Right msgKey -> do
           Db.run $ Sql.update (Sql.toSqlKey tradeChatId :: Sql.Key TradeChat) [TradeChatUpdatedAt Sql.=. time]
           return $ Sql.fromSqlKey msgKey
     else throwError $ Err.apiErr (Err.E401, Err.Unauthorized)

getMessages :: Int64 -> User -> App [Sql.Entity Message]
getMessages tradeChatId User {..} = do
  isGivenUserValid <- isInvolvedInChat tradeChatId userUsername
  if isGivenUserValid
    then Query.getMessages (Sql.toSqlKey tradeChatId)
    else throwError $ Err.apiErr (Err.E401, Err.Unauthorized)

isInvolvedInChat :: Int64 -> String -> App Bool
isInvolvedInChat tradeChatId username = do
  mbCurrentUserKey <- (fmap . fmap) Sql.entityKey (UserQuery.findByUsername username)
  mbCurrentUserTradeChats <-
    (fmap . fmap . fmap) Sql.fromSqlKey $ traverse TCQuery.findByUser mbCurrentUserKey
  return $ maybe False (tradeChatId `elem`) mbCurrentUserTradeChats
