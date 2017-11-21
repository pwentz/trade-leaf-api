module Queries.Offer where


import           Config               (App)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main as Db
import           Models.Offer
import           Models.User
import          Models.Request
import Models.Photo
import Models.Category

userOffers :: Sql.Key User -> App [Sql.Entity Offer]
userOffers userKey =
  Db.run $
    select $
      from $ \offers -> do
        where_ (offers ^. OfferUserId ==. val userKey)
        return offers

getOfferData :: Sql.Entity Offer -> App [(Sql.Entity Request, Value String, Sql.Entity Photo)]
getOfferData offer =
  Db.run $
    select $
      from $ \(requests `InnerJoin` offers  `InnerJoin` categories `InnerJoin` photos) -> do
          on (offers ^. OfferPhotoId ==. photos ^. PhotoId)
          on (offers ^. OfferCategoryId ==. categories ^. CategoryId)
          on (offers ^. OfferId ==. requests ^. RequestOfferId)
          where_ (offers ^. OfferId ==. val (Sql.entityKey offer))
          limit 1
          return (requests, (categories ^. CategoryName), photos)
