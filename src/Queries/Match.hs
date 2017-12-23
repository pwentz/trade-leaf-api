module Queries.Match
    ( findMatches
    , matchesByUser
    , findUserMatches
    ) where

import           Config               (App)
import           Control.Applicative  (liftA2)
import           Data.Coords          (distanceInMiles)
import           Data.Maybe           (fromMaybe)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import qualified Db.Main              as Db
import           Models.Offer
import           Models.Request
import           Models.User
import           Queries.Request      (getOfferRequest)

{-| find all matches -}
findMatches :: Sql.Entity User -> App [Sql.Entity Offer]
findMatches user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (requests ^. RequestCategoryId ==. offers ^. OfferCategoryId)
                where_ (users ^. UserId ==. val (Sql.entityKey user))
                return requests
    in Db.run $
       select $
       distinct $
       from $ \offers -> do
           userReq <- userReqs user
           where_ (offers ^. OfferId ==. userReq ^. RequestOfferId)
           where_ (offers ^. OfferUserId !=. val (Sql.entityKey user))
           return offers

{-| find all matches and the user they belong to -}
matchesByUser :: Sql.Entity User -> App [(Sql.Entity Offer, Sql.Entity User)]
matchesByUser user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (offers ^. OfferId ==. requests ^. RequestOfferId)
                where_ (users ^. UserId ==. val (Sql.entityKey user))
                return requests
    in Db.run $
       select $
       from $ \(offers `InnerJoin` users) -> do
           on (offers ^. OfferUserId ==. users ^. UserId)
           userReq <- userReqs user
           where_ (userReq ^. RequestCategoryId ==. offers ^. OfferCategoryId)
           where_ (users ^. UserId !=. val (Sql.entityKey user))
           return (offers, users)

{-| Finds all matches for offer belonging to user -}
findUserMatches :: Sql.Entity User -> Sql.Entity Offer -> App [Sql.Entity Offer]
findUserMatches currentUser offer =
    Db.run $
    select $
    from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
        on (offers ^. OfferUserId ==. users ^. UserId)
        on (requests ^. RequestOfferId ==. offers ^. OfferId)
        offerReq <-
            from $ \(reqs `InnerJoin` offers) -> do
                on (reqs ^. RequestOfferId ==. offers ^. OfferId)
                where_ (offers ^. OfferId ==. val (Sql.entityKey offer))
                return reqs
        where_ (users ^. UserId ==. val (Sql.entityKey currentUser))
        where_ (offers ^. OfferCategoryId ==. offerReq ^. RequestCategoryId)
        return offers
