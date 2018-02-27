{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Api.TradeChat where

import           Api.Error                   (StatusCode (..), apiErr, sqlError)
import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Int                    (Int64)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Offer
import           Models.TradeChat
import           Servant

data TradeChatRequest = TradeChatRequest
  { tradeId :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON TradeChatRequest

instance FromJSON TradeChatRequest

type TradeChatAPI
   = "trade-chat" :> ReqBody '[JSON] TradeChatRequest :> Post '[JSON] Int64

tradeChatServer :: ServerT TradeChatAPI App
tradeChatServer = createTradeChat

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
