module Queries.Offer where

import           Config               (App)
import           Control.Monad        (join)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main              as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.Trade
import           Models.TradeChat
import           Models.User
import           Queries.Request      (destroyRequest, getOfferRequest)
import           Queries.Trade        (destroyTrade, findFromOffer)
import           Queries.TradeChat    (destroyTradeChat, findByTrade)
import           Utils                (sHead)

userOffers :: Sql.Key User -> App [Sql.Entity Offer]
userOffers userKey =
  Db.run $
  select $
  from $ \offers -> do
    where_ (offers ^. OfferUserId ==. val userKey)
    return offers

{-| Using `head` since userId, categoryId, and photoId are all required fields on offer -}
getOfferData :: Sql.Entity Offer -> App (Sql.Entity User, Value String, Sql.Entity Photo)
getOfferData = (head <$>) . getData . entityKey
  where
    getData :: Sql.Key Offer -> App [(Sql.Entity User, Value String, Sql.Entity Photo)]
    getData offerKey =
      Db.run $
      select $
      from $ \(users `InnerJoin` offers `InnerJoin` categories `InnerJoin` photos) -> do
        on (offers ^. OfferPhotoId ==. photos ^. PhotoId)
        on (offers ^. OfferCategoryId ==. categories ^. CategoryId)
        on (offers ^. OfferUserId ==. users ^. UserId)
        where_ (offers ^. OfferId ==. val offerKey)
        limit 1
        return (users, categories ^. CategoryName, photos)

destroyOffer :: Sql.Key Offer -> App ()
destroyOffer offerKey = do
  mbTrade <- findFromOffer offerKey
  mbTradeChat <- join <$> traverse (findByTrade . Sql.entityKey) mbTrade
  mbReq <- getOfferRequest offerKey
  mbPhoto <- sHead <$> offerPhotos
  traverse (destroyTradeChat . Sql.entityKey) mbTradeChat
  traverse (destroyTrade . Sql.entityKey) mbTrade
  traverse (destroyRequest . Sql.entityKey) mbReq
  traverse (destroyPhoto . Sql.entityKey) mbPhoto
  Db.run $ Sql.deleteWhere [OfferId Sql.==. offerKey]
  return ()
  where
    destroyPhoto :: Sql.Key Photo -> App ()
    destroyPhoto photoKey = Db.run $ Sql.deleteWhere [PhotoId Sql.==. photoKey]
    offerPhotos =
      Db.run $
      select $
      from $ \(offers `InnerJoin` photos) -> do
        on (offers ^. OfferPhotoId ==. photos ^. PhotoId)
        where_ (offers ^. OfferId ==. val offerKey)
        return photos
