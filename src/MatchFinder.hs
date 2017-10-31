module MatchFinder where

import Config (App)
import Control.Applicative (liftA2)
import Coords (Coords, distanceInMiles, toCoords)
import Data.Maybe (fromMaybe)
import Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import Debug.Trace
import Models

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

{-| Finds all matches for offer belonging to user -}
findUserMatches :: User -> Offer -> App [Sql.Entity Offer]
findUserMatches currentUser offer =
    runDb $
    select $
    from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
        on (offers ^. OfferUserId ==. users ^. UserId)
        on (requests ^. RequestOfferId ==. offers ^. OfferId)
        where_ (users ^. UserUsername ==. val (userUsername currentUser))
        where_ (requests ^. RequestCategoryId ==. val (offerCategoryId offer))
        return offers

{-|
    TODO:
      - Iterate through matchesByUser (same way we're doing here)
      - for each offer, use findUserMatches passing current user and offer
        - this will give all matching offers belonging to the user
        - iterate through these and make sure that the distance between the users
          is less than at least 1 of them
      - if distance between users is less than at least one of those in list, then
        append offer to front of acc
      - otherwise return acc

    Because we're going to be bringing App into the folding function, it would be
    best for foldMatches to return an (App [..]) so it would be:

    foldMatches :: (Sql.Entity Offer, Sql.Entity User) -> App [Sql.Entity Offer] -> App [Sql.Entity Offer]
-}
findWithinRadius ::
       User -> [(Sql.Entity Offer, Sql.Entity User)] -> App [Sql.Entity Offer]
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
