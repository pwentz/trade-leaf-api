{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Match where

import           Api.Offer            (OfferResponse, toOfferResponse)
import           Api.User             (UserMeta (..), getUserMeta)
import           Config               (App)
import           Control.Applicative  (liftA2)
import           Data.Aeson           (ToJSON)
import           Data.Coords          (distanceInMiles)
import           Data.Maybe           (fromMaybe)
import qualified Database.Persist.Sql as Sql
import           GHC.Generics         (Generic)
import           Models.Offer
import           Models.User
import           Queries.Match
import           Servant

data MatchResponse = MatchResponse
  { offer    :: OfferResponse
  , user     :: UserMeta
  , distance :: Int
  } deriving (Show, Generic)

instance ToJSON MatchResponse

type MatchAPI
    = "matches" :> AuthProtect "jwt-auth" :> Get '[JSON] [MatchResponse]

matchServer :: ServerT MatchAPI App
matchServer = getMatches

getMatches :: User -> App [MatchResponse]
getMatches currentUser =
  matchesByUser currentUser >>= findWithinRadius currentUser

findWithinRadius :: User -> [(Sql.Entity Offer, Sql.Entity User)] -> App [MatchResponse]
findWithinRadius currentUser =
    foldr foldMatches (return [])
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
                distance <= offerRadius (Sql.entityVal offerEntity)
        in do
          currentUserMatches <- findUserMatches currentUser (Sql.entityVal offer)
          userMeta <- getUserMeta (Sql.fromSqlKey $ Sql.entityKey user)
          if isWithinDistance offer && any isWithinDistance currentUserMatches
              then do
                offerRes <- toOfferResponse offer
                (MatchResponse offerRes userMeta (round distance) :) <$> acc
              else acc
