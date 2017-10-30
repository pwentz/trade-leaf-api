module MatchFinder where

import           Config               (App)
import           Control.Applicative  (liftA2)
import           Coords               (Coords, distanceInMiles, toCoords)
import           Data.Maybe           (fromMaybe)
import           Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import           Debug.Trace
import           Models

{-|
  select offer.* from
    (select request.*
     from offer
     inner join public.user on public.user.id = offer.user_id
     inner join request on request.category_id = offer.category_id
     where public.user.username = 'pat'
    ) as myReqs
  inner join offer on offer.id = myReqs.offer_id;
-}
findMatches :: User -> App [Sql.Entity Offer]
findMatches user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (requests ^. RequestCategoryId ==. offers ^. OfferCategoryId)
                where_ (users ^. UserUsername ==. val (userUsername user))
                return requests
    in runDb $
       select $
       from $ \offers -> do
           userReq <- userReqs user
           where_ (offers ^. OfferId ==. userReq ^. RequestOfferId)
           return offers

findMatchesInRadius :: User -> App [(Sql.Entity Offer, Sql.Entity User)]
findMatchesInRadius user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (offers ^. OfferId ==. requests ^. RequestOfferId)
                where_ (users ^. UserUsername ==. val (userUsername user))
                return requests
    in runDb $
       select $
       from $ \(offers `InnerJoin` users) -> do
           on (offers ^. OfferUserId ==. users ^. UserId)
           userReq <- userReqs user
           where_ (userReq ^. RequestCategoryId ==. offers ^. OfferCategoryId)
           where_ (users ^. UserUsername !=. val (userUsername user))
           return (offers, users)

{-| NEED:

    [(Sql.Entity Offer, [(Sql.Entity Offer, Sql.Entity User)]]
    -- For each offer that belongs to user and has matching offers
    -- List of offerUsers
-}
findWithinRadius :: User -> [(Sql.Entity Offer, Sql.Entity User)] -> App [Sql.Entity Offer]
findWithinRadius currentUser matchesByUser =
    return $ (foldr foldMatches [] matchesByUser)
  where
    distanceBetweenUsers :: User -> User -> Double
    distanceBetweenUsers user1 user2 =
        fromMaybe (1 / 0) $
        liftA2
            distanceInMiles
            (toCoords $ userCoordinates user1)
            (toCoords $ userCoordinates user2)
    foldMatches ::
           (Sql.Entity Offer, Sql.Entity User)
        -> [Sql.Entity Offer]
        -> [Sql.Entity Offer]
    foldMatches (offer, user) acc =
        let distance = distanceBetweenUsers currentUser (Sql.entityVal user)
        in if distance <= (offerRadius $ Sql.entityVal offer)
               then offer : acc
               else acc
