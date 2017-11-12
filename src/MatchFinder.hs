module MatchFinder
  ( findMatchesWithinRadius
  , findMatches
  , matchesByUser
  , findUserMatches
  ) where

import           Config               (App)
import           Control.Applicative  (liftA2)
import           Data.Coords          (distanceInMiles)
import           Data.Maybe           (fromMaybe)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import           Models.User
import           Models.Offer
import           Models.Request
import qualified Db.Main as Db


{-| find all matches within radius -}
findMatchesWithinRadius :: User -> App [Sql.Entity Offer]
findMatchesWithinRadius currentUser =
  matchesByUser currentUser >>= findWithinRadius currentUser

{-| find all matches -}
findMatches :: User -> App [Sql.Entity Offer]
findMatches user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (requests ^. RequestCategoryId ==. offers ^. OfferCategoryId)
                where_ (users ^. UserUsername ==. val (userUsername user))
                return requests
    in Db.run $
       select $
       from $ \offers -> do
           userReq <- userReqs user
           where_ (offers ^. OfferId ==. userReq ^. RequestOfferId)
           return offers

{-| find all matches and the user they belong to -}
matchesByUser :: User -> App [(Sql.Entity Offer, Sql.Entity User)]
matchesByUser user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (offers ^. OfferId ==. requests ^. RequestOfferId)
                where_ (users ^. UserUsername ==. val (userUsername user))
                return requests
    in Db.run $
       select $
       from $ \(offers `InnerJoin` users) -> do
           on (offers ^. OfferUserId ==. users ^. UserId)
           userReq <- userReqs user
           where_ (userReq ^. RequestCategoryId ==. offers ^. OfferCategoryId)
           where_ (users ^. UserUsername !=. val (userUsername user))
           return (offers, users)

{-| Finds all matches for offer belonging to user -}
findUserMatches :: User -> Offer -> App [Sql.Entity Offer]
findUserMatches currentUser offer =
    Db.run $
    select $
    from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
        on (offers ^. OfferUserId ==. users ^. UserId)
        on (requests ^. RequestOfferId ==. offers ^. OfferId)
        where_ (users ^. UserUsername ==. val (userUsername currentUser))
        where_ (requests ^. RequestCategoryId ==. val (offerCategoryId offer))
        return offers

findWithinRadius :: User -> [(Sql.Entity Offer, Sql.Entity User)] -> App [Sql.Entity Offer]
findWithinRadius currentUser matchesByUser =
    foldr foldMatches (return []) matchesByUser
  where
    distanceBetweenUsers :: User -> User -> Double
    distanceBetweenUsers user1 user2 =
      fromMaybe (1/0) $
        liftA2
          distanceInMiles
          (userCoordinates user1)
          (userCoordinates user2)
    foldMatches :: (Sql.Entity Offer, Sql.Entity User) -> App [Sql.Entity Offer] -> App [Sql.Entity Offer]
    foldMatches (offer, user) acc =
        let distance = distanceBetweenUsers currentUser (Sql.entityVal user)
            isWithinDistance offerEntity =
                distance <= (offerRadius $ Sql.entityVal offerEntity)
        in do
          currentUserMatches <- findUserMatches currentUser (Sql.entityVal offer)
          if isWithinDistance offer && any isWithinDistance currentUserMatches
              then (offer :) <$> acc
              else acc
