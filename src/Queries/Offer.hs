module Queries.Offer where


import           Config               (App)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main as Db
import           Models.Offer
import           Models.User

userOffers :: Sql.Key User -> App [Sql.Entity Offer]
userOffers userKey =
  Db.run $
    select $
      from $ \offers -> do
        where_ (offers ^. OfferUserId ==. val userKey)
        return offers
