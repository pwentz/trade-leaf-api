{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Match where

import           Api.Offer            (OfferResponse (id), toOfferResponse)
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
import           Queries.Trade        (findAccepted, findExchange)
import           Servant
import           Utils

data ExchangeOffer = ExchangeOffer
    { offer      :: OfferResponse
    , isAccepted :: Bool
    } deriving (Show, Generic)

data MatchResponse = MatchResponse
    { offer          :: OfferResponse
    , user           :: UserMeta
    , exchangeOffers :: [ExchangeOffer]
    , distance       :: Int
    } deriving (Show, Generic)

instance ToJSON ExchangeOffer

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
        in do currentUserMatches <-
                  findUserMatches currentUser (Sql.entityVal offer)
              userMeta <- getUserMeta (Sql.fromSqlKey $ Sql.entityKey user)
              hasOfferBeenAccepted <- containsExchangeOffer offer currentUserMatches
              acceptedUserOfferTrade <- findAcceptedTrade offer currentUserMatches
              if isWithinDistance offer &&
                 any isWithinDistance currentUserMatches &&
                 not hasOfferBeenAccepted &&
                 not (isAcceptedUserTradeMutual acceptedUserOfferTrade)
                  then do
                      offerRes <- toOfferResponse offer
                      exchangeOffers <- traverse toOfferResponse currentUserMatches
                      case acceptedUserOfferTrade of
                          Nothing ->
                              (++ [ MatchResponse
                                    { offer = offerRes
                                    , user = userMeta
                                    , exchangeOffers =
                                          flip ExchangeOffer False <$>
                                          exchangeOffers
                                    , distance = round distance
                                    }
                                  ]) <$> acc
                          Just trade ->
                              (MatchResponse
                               { offer = offerRes
                               , user = userMeta
                               , exchangeOffers =
                                     acceptedExchangeOffer trade exchangeOffers
                               , distance = round distance
                               } :) <$> acc
                  else acc
    isAcceptedUserTradeMutual :: Maybe (Sql.Entity Trade) -> Bool
    isAcceptedUserTradeMutual =
        fromMaybe False . ((tradeIsMutual . Sql.entityVal) <$>)
    acceptedExchangeOffer :: Sql.Entity Trade -> [OfferResponse] -> [ExchangeOffer]
    acceptedExchangeOffer acceptedTrade =
      let
        acceptedOfferKey =
          Sql.fromSqlKey . tradeAcceptedOfferId . Sql.entityVal
      in
        foldr
            (\offerRes acc ->
                 ExchangeOffer
                 { offer = offerRes
                 , isAccepted =
                       Api.Offer.id offerRes == acceptedOfferKey acceptedTrade
                 } : acc)
            []

{-| Find all trades where given offer is approved offer, go through them and
    see if any potential matches (comparing offers) match the other offer on the
    trade

    ie. check to see if given offer has already been accepted in exchange
    for any of other offers
|-}
containsExchangeOffer :: Sql.Entity Offer -> [Sql.Entity Offer] -> App Bool
containsExchangeOffer targetOffer comparingOffers = do
    acceptedOfferTrades <- findAccepted (Sql.entityKey targetOffer)
    return (any containsExchangeOffer acceptedOfferTrades)
  where
    containsExchangeOffer approvedOfferTrade =
        any (isExchangeOffer approvedOfferTrade) comparingOffers
    isExchangeOffer offerTrade comparingOffer =
        Sql.entityKey comparingOffer ==
        tradeExchangeOfferId (Sql.entityVal offerTrade)

{-| Find trade where accepted offer is within given list of offers AND
    the given offer is the exchange offer.
|-}
findAcceptedTrade :: Sql.Entity Offer -> [Sql.Entity Offer] -> App (Maybe (Sql.Entity Trade))
findAcceptedTrade targetOffer matchingOffers = do
    exchangeOfferTrades <- findExchange (Sql.entityKey targetOffer)
    return (sHead $ filter containsAcceptedOffer exchangeOfferTrades)
  where
    containsAcceptedOffer exchangeTrade =
        any (isAcceptedOffer exchangeTrade) matchingOffers
    isAcceptedOffer trade comparingOffer =
        Sql.entityKey comparingOffer ==
        tradeAcceptedOfferId (Sql.entityVal trade)
