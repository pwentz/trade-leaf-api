{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Message where

import           Api.Error              (StatusCode (..), apiErr, sqlError)
import           Config                 (App)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Int               (Int64)
import           Data.Time              (getCurrentTime)
import qualified Database.Persist.Sql   as Sql
import qualified Db.Main                as Db
import           GHC.Generics           (Generic)
import           Models.Message
import           Servant

data MessageRequest = MessageRequest
  { tradeChatId :: Int64
  , senderId    :: Int64
  , content     :: String
  } deriving (Eq, Show, Generic)

instance FromJSON MessageRequest

type MessageAPI
    = "messages" :> ReqBody '[JSON] MessageRequest :> Post '[JSON] Int64

messageServer :: ServerT MessageAPI App
messageServer = createMessage

createMessage :: MessageRequest -> App Int64
createMessage MessageRequest {..} = do
  time <- liftIO getCurrentTime
  eitherMsg <-
    Db.runSafe $
    Sql.insert
      Message
      { messageTradeChatId = Sql.toSqlKey tradeChatId
      , messageSenderId = Sql.toSqlKey senderId
      , messageContent = content
      , messageCreatedAt = time
      , messageUpdatedAt = time
      }
  either (throwError . apiErr . (,) E400 . sqlError) (return . Sql.fromSqlKey) eitherMsg
