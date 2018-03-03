module Queries.TradeChat where

import           Config               (App)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main              as Db
import           Models.Offer
import           Models.Trade
import           Models.TradeChat
import           Models.User
import           Utils                (sHead)

findByTrade :: Sql.Key Trade -> App (Maybe (Sql.Entity TradeChat))
findByTrade tradeKey = sHead <$> foundTradeChat
  where
    foundTradeChat =
      Db.run $
        select $
          from $ \(trades `InnerJoin` tradeChats) -> do
            on (trades ^. TradeId ==. tradeChats ^. TradeChatTradeId)
            return tradeChats

findByUser :: Sql.Key User -> App [Sql.Key TradeChat]
findByUser = (fmap . fmap) unValue . getTradeChats
  where
    getTradeChats :: Sql.Key User -> App [Value (Sql.Key TradeChat)]
    getTradeChats userKey =
      Db.run $
        select $
          from $ \(tradeChats `InnerJoin` trades `InnerJoin` offers `InnerJoin` users) -> do
            on (users ^. UserId ==. offers ^. OfferUserId)
            on
              (offers ^. OfferId ==. trades ^. TradeAcceptedOfferId ||.
               offers ^. OfferId ==. trades ^. TradeExchangeOfferId)
            on (trades ^. TradeId ==. tradeChats ^. TradeChatTradeId)
            where_ (users ^. UserId ==. val userKey)
            return (tradeChats ^. TradeChatId)
