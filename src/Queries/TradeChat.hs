{-# LANGUAGE DeriveGeneric         #-}
module Queries.TradeChat where

import           Config               (App)
import           Control.Applicative
import           Data.Int             (Int64)
import qualified Data.Map             as Map
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main              as Db
import           Models.Message
import           Models.Offer
import           Models.Trade
import           Models.TradeChat
import           Models.User
import qualified Queries.Message      as MsgQuery
import           Utils                (sHead)
import GHC.Generics (Generic)

data ChatData =
  ChatData { recipient :: Int64
           , messages  :: [Sql.Entity Message]
           } deriving (Eq, Show, Generic)

findByTrade :: Sql.Key Trade -> App (Maybe (Sql.Entity TradeChat))
findByTrade tradeKey = sHead <$> foundTradeChat
  where
    foundTradeChat =
      Db.run $
        select $
          from $ \(trades `InnerJoin` tradeChats) -> do
            on (trades ^. TradeId ==. tradeChats ^. TradeChatTradeId)
            where_ (trades ^. TradeId ==. val tradeKey)
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

findChatData :: Sql.Key User -> App (Map.Map Int64 ChatData)
findChatData userKey =
   foldr
    (\tradeChatKey ->
      liftA2
        (Map.insert (Sql.fromSqlKey tradeChatKey))
        (liftA2
          (ChatData . Sql.fromSqlKey . recipient)
          (findRecipients tradeChatKey)
          (MsgQuery.getMessages tradeChatKey)
        )
    )
    (return Map.empty) =<< findByUser userKey
     where
         recipient :: (Sql.Key User, Sql.Key User) -> Sql.Key User
         recipient (acceptedUserKey, exchangeUserKey)
             | userKey == acceptedUserKey = exchangeUserKey
             | otherwise = acceptedUserKey

findRecipients :: Sql.Key TradeChat -> App (Sql.Key User, Sql.Key User)
findRecipients tradeChatKey = do
  (acceptedUserKey:exchangeUserKey:_) <- getUsers tradeChatKey
  return (unValue acceptedUserKey, unValue exchangeUserKey)
  where
      getUsers :: Sql.Key TradeChat -> App [Value (Sql.Key User)]
      getUsers tcKey =
        Db.run $
          select $
            from $ \(tradeChats `InnerJoin` trades `InnerJoin` offers `InnerJoin` users) -> do
              on (users ^. UserId ==. offers ^. OfferUserId)
              on
                (offers ^. OfferId ==. trades ^. TradeAcceptedOfferId ||.
                 offers ^. OfferId ==. trades ^. TradeExchangeOfferId)
              on (trades ^. TradeId ==. tradeChats ^. TradeChatTradeId)
              where_ (tradeChats ^. TradeChatId ==. val tcKey)
              return (users ^. UserId)

findAllChatData :: Sql.Key User -> App [(Sql.Entity TradeChat, Sql.Entity Trade, Sql.Entity Offer, Sql.Entity User, Sql.Entity Message)]
findAllChatData userKey =
  let
    chatMessages =
      from $ \(messages) -> do
          orderBy [asc (messages ^. MessageCreatedAt)]
          return messages
  in
  Db.run $
    select $
      from $ \(tradeChats `InnerJoin` trades `InnerJoin` offers `InnerJoin` users) -> do
        on (users ^. UserId ==. offers ^. OfferUserId)
        on
          (offers ^. OfferId ==. trades ^. TradeAcceptedOfferId ||.
           offers ^. OfferId ==. trades ^. TradeExchangeOfferId)
        on (trades ^. TradeId ==. tradeChats ^. TradeChatTradeId)
        orderedMessages <- chatMessages
        where_ (users ^. UserId ==. val userKey)
        return (tradeChats, trades, offers, users, orderedMessages)

destroyTradeChat :: Sql.Key TradeChat -> App ()
destroyTradeChat tradeChatKey = Db.run $ Sql.deleteWhere [TradeChatId Sql.==. tradeChatKey]
