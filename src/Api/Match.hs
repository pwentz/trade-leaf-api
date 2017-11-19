{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Match where

import           Api.User             (UserMeta (..), getUserMeta)
import Api.Offer (toOfferResponse, OfferResponse)
import           Config               (App)
import           Control.Applicative  (liftA2)
import           Data.Coords          (distanceInMiles)
import           Data.Maybe           (fromMaybe)
import qualified Database.Persist.Sql as Sql
import           Models.Offer
import           Models.User
import           Queries.Match

data MatchResponse = MatchResponse
  { offer    :: OfferResponse
  , user     :: UserMeta
  , distance :: Int
  }


getMatches :: User -> App [MatchResponse]
getMatches currentUser =
  matchesByUser currentUser >>= findWithinRadius currentUser

findWithinRadius :: User -> [(Sql.Entity Offer, Sql.Entity User)] -> App [MatchResponse]
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
    foldMatches :: (Sql.Entity Offer, Sql.Entity User) -> App [MatchResponse] -> App [MatchResponse]
    foldMatches (offer, user) acc =
        let distance = distanceBetweenUsers currentUser (Sql.entityVal user)
            isWithinDistance offerEntity =
                distance <= (offerRadius $ Sql.entityVal offerEntity)
        in do
          currentUserMatches <- findUserMatches currentUser (Sql.entityVal offer)
          userMeta <- getUserMeta (Sql.fromSqlKey $ Sql.entityKey user)
          if isWithinDistance offer && any isWithinDistance currentUserMatches
              then
                toOfferResponse offer >>=
                  maybe acc (\offerRes ->
                    ((MatchResponse offerRes userMeta (round distance)) :) <$> acc)
              else acc
