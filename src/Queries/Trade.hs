module Queries.Trade where

import           Config                      (App)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Offer
import           Models.Trade
import           Utils                       (sHead)

findFromOffers :: Pg.Key Offer -> Pg.Key Offer -> App (Maybe (Pg.Entity Trade))
findFromOffers offer1Key offer2Key = sHead <$> foundTrades
  where
    foundTrades =
        Db.run $
        select $
        from $ \trades -> do
            where_
                ((trades ^. TradeOffer1Id ==. val offer1Key) ||.
                 (trades ^. TradeOffer2Id ==. val offer1Key))
            where_
                ((trades ^. TradeOffer1Id ==. val offer2Key) ||.
                 (trades ^. TradeOffer2Id ==. val offer2Key))
            return trades
