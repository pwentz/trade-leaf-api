{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Api.TradeChat where

import           Api.Error                   (StatusCode (..), ApiErr(RequestedUserNotAuth), apiErr, sqlError)
import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Int                    (Int64)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Offer
import Data.Maybe (fromMaybe)
import           Models.TradeChat
import           Models.User
import           Servant
import qualified Data.Map as Map
import qualified Queries.TradeChat as Query
import qualified Queries.User as UserQuery

data TradeChatRequest = TradeChatRequest
  { tradeId :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON TradeChatRequest
instance FromJSON TradeChatRequest

instance ToJSON Query.ChatData

type TradeChatAPI
   = "trade-chat" :> ReqBody '[JSON] TradeChatRequest :> Post '[JSON] Int64
   :<|> "trade-chat" :> Capture "id" Int64 :> AuthProtect "jwt-auth" :> Get '[JSON] (Map.Map Int64 Query.ChatData)

-- tradeChatServer :: ServerT TradeChatAPI App
tradeChatServer = createTradeChat :<|> fetchChatData

createTradeChat :: TradeChatRequest -> App Int64
createTradeChat TradeChatRequest {..} = do
  time <- liftIO getCurrentTime
  eitherTradeChat <-
    Db.runSafe $
    Sql.insert
      (TradeChat (Sql.toSqlKey tradeId) time time)
  either
    (throwError . apiErr . (,) E400 . sqlError)
    (return . Sql.fromSqlKey)
    eitherTradeChat

fetchChatData :: Int64 -> User -> App (Map.Map Int64 Query.ChatData)
fetchChatData userId currentUser = do
  requestedUser <- UserQuery.get userId
  if fromMaybe False ((== currentUser) <$> requestedUser)
     then Query.findChatData (Sql.toSqlKey userId)
     else throwError $ apiErr (E401, RequestedUserNotAuth)
