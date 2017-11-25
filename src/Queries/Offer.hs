module Queries.Offer where


import           Config               (App)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main              as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.User

userOffers :: Sql.Key User -> App [Sql.Entity Offer]
userOffers userKey =
  Db.run $
    select $
      from $ \offers -> do
        where_ (offers ^. OfferUserId ==. val userKey)
        return offers

{-| Using `head` since userId, categoryId, and photoId are all required fields on offer -}
getOfferData :: Sql.Entity Offer -> App (Sql.Entity User, Value String, Sql.Entity Photo)
getOfferData =
  (head <$>) . getData . entityKey
  where
    getData :: Sql.Key Offer -> App [(Sql.Entity User, Value String, Sql.Entity Photo)]
    getData offerKey =
      Db.run $
        select $
          from $ \(users `InnerJoin` offers  `InnerJoin` categories `InnerJoin` photos) -> do
              on (offers ^. OfferPhotoId ==. photos ^. PhotoId)
              on (offers ^. OfferCategoryId ==. categories ^. CategoryId)
              on (offers ^. OfferUserId ==. users ^. UserId)
              where_ (offers ^. OfferId ==. val offerKey)
              limit 1
              return (users, categories ^. CategoryName, photos)
