{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Match where

import           Api.Offer            (OfferResponse (id), toOfferResponse)
import           Api.User             (UserMeta (..), getUserMeta)
import           Config               (App)
import           Control.Applicative  (liftA2)
import           Control.Monad        (join)
import           Data.Aeson           (ToJSON)
import           Data.Coords          (distanceInMiles)
import           Data.List            (nub)
import           Data.Maybe           (fromMaybe)
import qualified Database.Persist.Sql as Sql
import           Debug.Trace          (trace)
import           GHC.Generics         (Generic)
import           Models.Offer
import           Models.Trade
import           Models.TradeChat
import           Models.User
import           Queries.Match
import           Queries.Trade        (findAccepted, findExchange,
                                       findFromInvolved)
import           Queries.TradeChat    (findByTrade)
import           Queries.User         (findByUsername)
import           Servant
import           Utils

data ExchangeOffer = ExchangeOffer
    { offer      :: OfferResponse
    , isAccepted :: Bool
    } deriving (Eq, Show, Generic)

data MatchResponse = MatchResponse
    { offer          :: OfferResponse
    , user           :: UserMeta
    , exchangeOffers :: [ExchangeOffer]
    , distance       :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON ExchangeOffer

instance ToJSON MatchResponse

type MatchAPI
     = "matches" :> AuthProtect "jwt-auth" :> Get '[JSON] [MatchResponse]

matchServer :: ServerT MatchAPI App
matchServer = getMatches

getMatches :: User -> App [MatchResponse]
getMatches currentUser = do
    mbUserKey <- (Sql.entityKey <$>) <$> findByUsername (userUsername currentUser)
    maybe (return []) (findMatches . flip Sql.Entity currentUser) mbUserKey
  where
    findMatches :: Sql.Entity User -> App [MatchResponse]
    findMatches =
      liftA2 (>>=) matchesByUser findWithinRadius

findWithinRadius :: Sql.Entity User -> [(Sql.Entity Offer, Sql.Entity User)] -> App [MatchResponse]
findWithinRadius currentUser = (nub <$>) . foldr foldMatches (return [])
  where
    distanceBetweenUsers :: User -> User -> Double
    distanceBetweenUsers user1 user2 =
        fromMaybe (1 / 0) $
        liftA2 distanceInMiles (userCoordinates user1) (userCoordinates user2)
    foldMatches :: (Sql.Entity Offer, Sql.Entity User) -> App [MatchResponse] -> App [MatchResponse]
    foldMatches (offer, user) acc =
        let distance = distanceBetweenUsers (Sql.entityVal currentUser) (Sql.entityVal user)
            isWithinDistance offerEntity =
                distance <= offerRadius (Sql.entityVal offerEntity)
        in do currentUserMatches <- findUserMatches currentUser offer
              isAlreadyInvolvedInTradeChatWithUser <- isInvolvedInTradeChat offer currentUserMatches
              if isWithinDistance offer &&
                 any isWithinDistance currentUserMatches &&
                 not isAlreadyInvolvedInTradeChatWithUser
                  then do
                      userMeta <- getUserMeta (Sql.fromSqlKey $ Sql.entityKey user)
                      acceptedUserOfferTrade <- findAcceptedTrade offer currentUserMatches
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

{-| Given a target offer and all of current user's offers, determine whether
    any of the current user's offers are involved in a trade chat with the given
    offer.

    ie. ensure that we're not already involved in a trade chat with this offer
|-}
isInvolvedInTradeChat :: Sql.Entity Offer -> [Sql.Entity Offer] -> App Bool
isInvolvedInTradeChat (Sql.Entity offerKey offerVal) comparingOffers = do
  traverse (findFromInvolved offerKey . Sql.entityKey) comparingOffers >>=
    fmap or . traverse doesTradeHaveChat
  where
    doesTradeHaveChat :: Maybe (Sql.Entity Trade) -> App Bool
    doesTradeHaveChat trade =
        maybe False (const True) . join <$> traverse (findByTrade . Sql.entityKey) trade


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
