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
findFromOffers offer1Key offer2Key = sHead <$> foundTrades
  where
    foundTrades =
      Db.run $
      select $
      from $ \trades -> do
        where_
          ((trades ^. TradeAcceptedOfferId ==. val offer1Key) ||.
           (trades ^. TradeExchangeOfferId ==. val offer1Key))
        where_
          ((trades ^. TradeAcceptedOfferId ==. val offer2Key) ||.
           (trades ^. TradeExchangeOfferId ==. val offer2Key))
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
