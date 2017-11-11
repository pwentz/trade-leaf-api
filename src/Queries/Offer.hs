module Queries.Offer where


import Config (App)
import Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import Models

userOffers :: Sql.Key User -> App [Sql.Entity Offer]
userOffers userKey =
  runDb $
    select $
      from $ \offers -> do
        where_ (offers ^. OfferUserId ==. val userKey)
        return offers
