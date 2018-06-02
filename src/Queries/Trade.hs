module Queries.Trade where

import           Config                      (App)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Offer
import           Models.Trade
import           Models.TradeChat
import           Utils                       (sHead)

findFromOffers :: Pg.Key Offer -> Pg.Key Offer -> App (Maybe (Pg.Entity Trade))
findFromOffers acceptedOfferKey exchangeOfferKey = sHead <$> foundTrades
  where
    foundTrades =
      Db.run $
      select $
      from $ \trades -> do
        where_
          ((trades ^. TradeAcceptedOfferId ==. val acceptedOfferKey) &&.
           (trades ^. TradeExchangeOfferId ==. val exchangeOfferKey))
        return trades

findAccepted :: Pg.Key Offer -> App [Pg.Entity Trade]
findAccepted offerKey =
  Db.run $
  select $
  from $ \(trades `InnerJoin` offers) -> do
    on (trades ^. TradeAcceptedOfferId ==. offers ^. OfferId)
    where_ (trades ^. TradeAcceptedOfferId ==. val offerKey)
    return trades

findExchange :: Pg.Key Offer -> App [Pg.Entity Trade]
findExchange offerKey =
  Db.run $
  select $
  from $ \(trades `InnerJoin` offers) -> do
    on (trades ^. TradeExchangeOfferId ==. offers ^. OfferId)
    where_ (trades ^. TradeExchangeOfferId ==. val offerKey)
    return trades

findFromInvolved :: Pg.Key Offer -> Pg.Key Offer -> App (Maybe (Pg.Entity Trade))
findFromInvolved acceptedOfferKey exchangeOfferKey = sHead <$> foundTrades
  where
    foundTrades =
      Db.run $
      select $
      from $ \trades -> do
        where_
          (
            ((trades ^. TradeAcceptedOfferId ==. val acceptedOfferKey) &&.
              (trades ^. TradeExchangeOfferId ==. val exchangeOfferKey)) ||.
            ((trades ^. TradeAcceptedOfferId ==. val exchangeOfferKey) &&.
              (trades ^. TradeExchangeOfferId ==. val acceptedOfferKey))
          )
        return trades

findFromOffer :: Pg.Key Offer -> App (Maybe (Pg.Entity Trade))
findFromOffer offerKey =
  sHead <$> foundTrade
  where
      foundTrade =
        Db.run $
          select $
            from $ \trades -> do
            where_ (trades ^. TradeAcceptedOfferId ==. val offerKey ||.
                    trades ^. TradeExchangeOfferId ==. val offerKey)
            return trades

destroyTrade :: Pg.Key Trade -> App ()
destroyTrade tradeKey = Db.run $ Pg.deleteWhere [TradeId Pg.==. tradeKey]
