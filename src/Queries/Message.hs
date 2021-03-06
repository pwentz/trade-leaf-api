module Queries.Message where

import           Config               (App)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main              as Db
import           Models.Message
import           Models.TradeChat

getMessages :: Sql.Key TradeChat -> App [Sql.Entity Message]
getMessages tradeChatKey =
  Db.run $
    select $
      from $ \(messages `InnerJoin` tradeChats) -> do
          on (messages ^. MessageTradeChatId ==. tradeChats ^. TradeChatId)
          where_ (tradeChats ^. TradeChatId ==. val tradeChatKey)
          orderBy [desc (messages ^. MessageCreatedAt)]
          return messages

destroyMessage :: Sql.Key Message -> App ()
destroyMessage msgKey = Db.run $ Sql.deleteWhere [MessageId Sql.==. msgKey]
