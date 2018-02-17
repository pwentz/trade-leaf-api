{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Api.MatchSpec where

import           Api.Match
import           Api.Offer                   (OfferResponse (description, id))
import           Api.User                    (UserMeta (username))
import           Config                      (App)
import           Control.Applicative         (liftA2)
import           Control.Monad               (join)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coords                 (Coords (Coords))
import           Data.Time                   (UTCTime, getCurrentTime)
import           Database.Persist            (get)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.Trade
import           Models.TradeChat
import           Models.User
import           Queries.Match
import           SpecHelper                  (DbSetup (..), dbSetup, runAppToIO,
                                              setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

newTrade :: Sql.Key Offer -> Sql.Key Offer -> Maybe (Sql.Key TradeChat) -> UTCTime -> Trade
newTrade offer1Key offer2Key mbTradeChatKey time =
  Trade
    { tradeAcceptedOfferId = offer1Key
    , tradeExchangeOfferId = offer2Key
    , tradeTradeChatId = mbTradeChatKey
    , tradeIsSuccessful = False
    , tradeCreatedAt = time
    , tradeUpdatedAt = time
    }


spec :: Spec
spec =
  around setupTeardown $
    describe "Api.Matches" $ do
      context "containsExchangeOffer" $ do
        it "returns true if given offer is accepted offer in trade with any given offers" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          hasApproved <- runAppToIO config $
            let
              userOffers =
                [currentUserOffer1Key, currentUserOffer2Key, currentUserOffer3Key, currentUserOffer4Key]
            in do
            time <- liftIO getCurrentTime
            tradeChatKey <- Db.run $ Sql.insert (TradeChat user2OfferKey currentUserOffer1Key time time)
            _ <- Db.run $ Sql.insert (newTrade user2OfferKey currentUserOffer1Key (Just tradeChatKey) time)
            mbOffer <- (Sql.Entity user2OfferKey <$>) <$> Db.run (Sql.get user2OfferKey)
            mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
            let mbUserOfferEnts = zipWith Sql.Entity userOffers <$> mbUserOffers
            sequence (liftA2 containsExchangeOffer mbOffer mbUserOfferEnts)
          hasApproved `shouldBe` Just True
        it "returns false if given offer is NOT accepted offer in trade with any given offers" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          hasApproved <- runAppToIO config $
            let
              userOffers =
                [currentUserOffer1Key, currentUserOffer2Key, currentUserOffer3Key, currentUserOffer4Key]
            in do
            time <- liftIO getCurrentTime
            tradeChatKey <- Db.run $ Sql.insert (TradeChat currentUserOffer1Key user2OfferKey time time)
            _ <- Db.run $ Sql.insert (newTrade currentUserOffer1Key user2OfferKey (Just tradeChatKey) time)
            _ <- Db.run $ Sql.insert (newTrade currentUserOffer2Key currentUserOffer3Key Nothing time)
            mbOffer <- (Sql.Entity user2OfferKey <$>) <$> Db.run (Sql.get user2OfferKey)
            mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
            let mbUserOfferEnts = zipWith Sql.Entity userOffers <$> mbUserOffers
            sequence (liftA2 containsExchangeOffer mbOffer mbUserOfferEnts)
          hasApproved `shouldBe` Just False

      context "findAcceptedTrade" $
        it "finds trade w/ accepted offer within given list of offers where given offer is exchange offer" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          (acceptedTradeKey, expected) <- runAppToIO config $
            let
              userOffers =
                [currentUserOffer1Key, currentUserOffer2Key, currentUserOffer3Key, currentUserOffer4Key]
            in do
              time <- liftIO getCurrentTime
              trade1Key <- Db.run $ Sql.insert (newTrade currentUserOffer2Key user4OfferKey Nothing time)
              trade2Key <- Db.run $ Sql.insert (newTrade currentUserOffer3Key user2OfferKey Nothing time)
              mbOffer <- (Sql.Entity user4OfferKey <$>) <$> Db.run (Sql.get user4OfferKey)
              mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
              let mbUserOfferEnts = zipWith Sql.Entity userOffers <$> mbUserOffers
              acceptedOfferTrade <- join <$> sequence (liftA2 findAcceptedTrade mbOffer mbUserOfferEnts)
              return (Sql.entityKey <$> acceptedOfferTrade, trade1Key)
          acceptedTradeKey `shouldBe` Just expected

      context "getMatches" $ do
        it "can get all users for offers within a given radius" $ \config ->
          let
            expectedExchangeOfferDescriptions =
              [["wooden battleship", "wooden rowboat", "wooden canoe"], ["some painting"]]
          in do
          matches <- runAppToIO config $ do
              DbSetup {..} <- dbSetup
              currentUser <- Db.run (Sql.get currentUserKey)
              traverse getMatches currentUser
          (((\MatchResponse{..} -> description offer) <$>) <$> matches) `shouldBe` Just ["man in rain - watercolor", "water color 30x40 painting"]
          ((distance <$>) <$> matches) `shouldBe` Just [9, 6]
          ((username . user <$>) <$> matches) `shouldBe` Just ["milliebob", "freddyjohn"]
          (((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe` Just expectedExchangeOfferDescriptions
        it "excludes offers when they are in trade with current user's matching offer" $ \config -> do
          matches <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              DbSetup {..} <- dbSetup
              currentUser <- Db.run (Sql.get currentUserKey)
              tradeChatKey <- Db.run $ Sql.insert (TradeChat user2OfferKey currentUserOffer1Key time time)
              tradeKey <- Db.run $ Sql.insert (newTrade user2OfferKey currentUserOffer1Key (Just tradeChatKey) time)
              traverse getMatches currentUser
          (((\MatchResponse{..} -> description offer) <$>) <$> matches) `shouldBe` Just ["man in rain - watercolor"]
          ((distance <$>) <$> matches) `shouldBe` Just [9]
          (((username . user) <$>) <$> matches) `shouldBe` Just ["milliebob"]
          (((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe` Just [["wooden battleship", "wooden rowboat", "wooden canoe"]]
        it "pushes exchange offers to front of match stack (if trade is not mutual)" $ \config ->
          let
            expectedExchangeOfferDescriptions =
              [["some painting"], ["wooden battleship", "wooden rowboat", "wooden canoe"]]
          in do
          matches <- runAppToIO config $ do
            time <- liftIO getCurrentTime
            DbSetup {..} <- dbSetup
            currentUser <- Db.run (Sql.get currentUserKey)
            tradeKey <- Db.run $ Sql.insert (newTrade currentUserOffer1Key user2OfferKey Nothing time)
            traverse getMatches currentUser
          (((\MatchResponse{..} -> description offer) <$>) <$> matches) `shouldBe` Just ["water color 30x40 painting", "man in rain - watercolor"]
          ((distance <$>) <$> matches) `shouldBe` Just [6, 9]
          ((username . user <$>) <$> matches) `shouldBe` Just ["freddyjohn", "milliebob"]
          (((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe` Just expectedExchangeOfferDescriptions
          (((isAccepted <$>) . exchangeOffers <$>) <$> matches) `shouldBe` Just [[True], [False, False, False]]
        it "excludes exchange offers (if trade is mutual)" $ \config ->
          let
            expectedExchangeOfferDescriptions =
              [["wooden battleship", "wooden rowboat", "wooden canoe"]]
          in do
          matches <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              DbSetup {..} <- dbSetup
              currentUser <- Db.run (Sql.get currentUserKey)
              tradeChatKey <- Db.run $ Sql.insert (TradeChat currentUserOffer1Key user2OfferKey time time)
              _ <- Db.run $ Sql.insert (newTrade currentUserOffer1Key user2OfferKey (Just tradeChatKey) time)
              traverse getMatches currentUser
          (((\MatchResponse{..} -> description offer) <$>) <$> matches) `shouldBe` Just ["man in rain - watercolor"]
          ((distance <$>) <$> matches) `shouldBe` Just [9]
          ((username . user <$>) <$> matches) `shouldBe` Just ["milliebob"]
          (((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe` Just expectedExchangeOfferDescriptions
        it "does not return duplicate matches" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          (matches, offer4MatchKeys) <- runAppToIO config $ do
            currentUser <- Db.run (Sql.get currentUserKey)
            mbMatches <- traverse getMatches currentUser
            let offer4Matches = (exchangeOffers =<<) . filter (\MatchResponse{..} -> Api.Offer.id offer == Sql.fromSqlKey user4OfferKey) <$> mbMatches
                offer4MatchKeys = traverse ((\ExchangeOffer{..} -> Sql.toSqlKey $ Api.Offer.id offer) <$>) offer4Matches
            return (mbMatches, offer4MatchKeys)
          length <$> matches `shouldBe` Just 2
          offer4MatchKeys `shouldBe` Just <$> [currentUserOffer2Key, currentUserOffer3Key, currentUserOffer4Key]
