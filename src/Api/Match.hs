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
import           Models.Trade
import           Models.User
import           Queries.Match
import           Queries.Trade        (findAccepted)
import           Servant

data MatchResponse = MatchResponse
    { offer          :: OfferResponse
    , user           :: UserMeta
    , exchangeOffers :: [OfferResponse]
    , distance       :: Int
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
findWithinRadius currentUser = foldr foldMatches (return [])
  where
    distanceBetweenUsers :: User -> User -> Double
    distanceBetweenUsers user1 user2 =
        fromMaybe (1 / 0) $
        liftA2 distanceInMiles (userCoordinates user1) (userCoordinates user2)
    foldMatches :: (Sql.Entity Offer, Sql.Entity User) -> App [MatchResponse] -> App [MatchResponse]
    foldMatches (offer, user) acc =
        let distance = distanceBetweenUsers currentUser (Sql.entityVal user)
            isWithinDistance offerEntity =
                distance <= offerRadius (Sql.entityVal offerEntity)
        in do currentUserMatches <- findUserMatches currentUser (Sql.entityVal offer)
              userMeta <- getUserMeta (Sql.fromSqlKey $ Sql.entityKey user)
              hasOfferBeenAccepted <- hasAcceptedOffer offer currentUserMatches
              if isWithinDistance offer && any isWithinDistance currentUserMatches && not hasOfferBeenAccepted
                  then do
                      offerRes <- toOfferResponse offer
                      -- if any of exchangeOffers are an `approvedOffer` on match/trade record
                      -- where offer is other offer AND trade is still open, then move those to front
                      -- (default is to append others to back)
                      exchangeOffers <-
                          traverse toOfferResponse currentUserMatches
                      (MatchResponse
                       { offer = offerRes
                       , user = userMeta
                       , exchangeOffers = exchangeOffers
                       , distance = round distance
                       } :) <$>
                          acc
                  else acc

{-| Find all trades where given offer is approved offer, go through them and
    see if any potential matches (comparing offers) match the other offer on the
    trade

    ie. check to see if given offer has already been accepted in exchange
    for any of other offers
|-}
hasAcceptedOffer :: Sql.Entity Offer -> [Sql.Entity Offer] -> App Bool
hasAcceptedOffer targetOffer comparingOffers = do
    acceptedOfferTrades <- findAccepted (Sql.entityKey targetOffer)
    return (any containsExchangeOffer acceptedOfferTrades)
  where
    containsExchangeOffer approvedOfferTrade =
        any (isExchangeOffer approvedOfferTrade) comparingOffers
    isExchangeOffer offerTrade comparingOffer =
        Sql.entityKey comparingOffer ==
        tradeExchangeOfferId (Sql.entityVal offerTrade)
