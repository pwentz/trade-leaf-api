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
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

data DbSetup = DbSetup
    { currentUserKey       :: Sql.Key User
    , currentUserOffer1Key :: Sql.Key Offer
    , currentUserOffer2Key :: Sql.Key Offer
    , currentUserOffer3Key :: Sql.Key Offer
    , currentUserOffer4Key :: Sql.Key Offer
    , user2Key             :: Sql.Key User
    , user2OfferKey        :: Sql.Key Offer
    , user3Key             :: Sql.Key User
    , user3OfferKey        :: Sql.Key Offer
    , user4Key             :: Sql.Key User
    , user4OfferKey        :: Sql.Key Offer
    }

dbSetup :: App DbSetup
dbSetup = do
    time <- liftIO getCurrentTime
    photoKey <- Db.createPhoto "" time
    woodworkingCategoryKey <- Db.createCategory "woodworking" time
    artCategoryKey <- Db.createCategory "decorative art" time
    currentUserKey <- Db.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time
    currentUserOffer1Key <- Db.createOffer currentUserKey artCategoryKey photoKey "some painting" 999 time
    currentUserRequest1Key <- Db.createRequest currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time

    {-| 6 miles from currentUser -}
    user2Key <- Db.createUser "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time
    user2OfferKey <- Db.createOffer user2Key artCategoryKey photoKey "water color 30x40 painting" 10 time
    user2OfferRequestKey <- Db.createRequest user2OfferKey artCategoryKey "animal painting for kid" time

    {-| 14 miles from currentUser -}
    user3Key <- Db.createUser "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" "password" Nothing (Just $ Coords 41.734517 (-87.674043)) time
    user3OfferKey <- Db.createOffer user3Key artCategoryKey photoKey "finger painting dog with lots of colors" 10 time
    user3OfferRequestKey <- Db.createRequest user3OfferKey artCategoryKey "looking for a large painting" time

    {-| 9 miles from currentUser -}
    user4Key <- Db.createUser "Millie" "Bobby Brown" "bobby@brown.com" "milliebob" "password" Nothing (Just $ Coords 41.804575 (-87.671359)) time
    user4OfferKey <- Db.createOffer user4Key artCategoryKey photoKey "man in rain - watercolor" 10 time
    user4OfferRequestKey <- Db.createRequest user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time

    currentUserOffer2Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time
    currentUserRequest2Key <- Db.createRequest currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time
    currentUserOffer3Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time
    currentUserRequest3Key <- Db.createRequest currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time
    currentUserOffer4Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time
    currentUserRequest4Key <- Db.createRequest currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time

    user5Key <- Db.createUser "John" "Johnson" "jj@gmail.com" "jj1" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time
    user5OfferKey <- Db.createOffer user5Key artCategoryKey photoKey "waterfall" 999 time
    user5OfferRequestKey <- Db.createRequest user5OfferKey artCategoryKey "looking for a cool painting" time

    tradeKey <- Db.createTrade currentUserOffer1Key user5OfferKey False time
    tradeChatKey <- Db.createTradeChat tradeKey time

    return DbSetup
            { currentUserKey = currentUserKey
            , currentUserOffer1Key = currentUserOffer1Key
            , currentUserOffer2Key = currentUserOffer2Key
            , currentUserOffer3Key = currentUserOffer3Key
            , currentUserOffer4Key = currentUserOffer4Key
            , user2Key = user2Key
            , user2OfferKey = user2OfferKey
            , user3Key = user3Key
            , user3OfferKey = user3OfferKey
            , user4Key = user4Key
            , user4OfferKey = user4OfferKey
            }

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "Api.Matches" $ do
    context "isInvolvedInTradeChat" $ do
      it "returns true if given offer is involved in trade chat with any of user's offers" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        hasApproved <-
          Spec.runAppToIO config $
          let userOffers =
                [ currentUserOffer1Key
                , currentUserOffer2Key
                , currentUserOffer3Key
                , currentUserOffer4Key
                ]
          in do time <- liftIO getCurrentTime
                tradeKey <- Db.createTrade user2OfferKey currentUserOffer1Key False time
                tradeChatKey <- Db.createTradeChat tradeKey time
                mbOffer <- fmap (Sql.Entity user2OfferKey) <$> Db.run (Sql.get user2OfferKey)
                mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
                let mbUserOfferEnts = zipWith Sql.Entity userOffers <$> mbUserOffers
                sequence (liftA2 isInvolvedInTradeChat mbOffer mbUserOfferEnts)
        hasApproved `shouldBe` Just True
      it "returns false if given offer is NOT involved in trade chat with any given offers" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        hasApproved <-
          Spec.runAppToIO config $
          let userOffers =
                [ currentUserOffer1Key
                , currentUserOffer2Key
                , currentUserOffer3Key
                , currentUserOffer4Key
                ]
          in do time <- liftIO getCurrentTime
                tradeKey <-
                  Db.createTrade currentUserOffer1Key user2OfferKey False time
                _ <- Db.createTrade currentUserOffer2Key currentUserOffer3Key False time
                mbOffer <- fmap (Sql.Entity user2OfferKey) <$> Db.run (Sql.get user2OfferKey)
                mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
                let mbUserOfferEnts = zipWith Sql.Entity userOffers <$> mbUserOffers
                sequence (liftA2 isInvolvedInTradeChat mbOffer mbUserOfferEnts)
        hasApproved `shouldBe` Just False
    context "findAcceptedTrade" $
      it
        "finds trade w/ accepted offer within given list of offers where given offer is exchange offer" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        (acceptedTradeKey, expected) <-
          Spec.runAppToIO config $
          let userOffers =
                [ currentUserOffer1Key
                , currentUserOffer2Key
                , currentUserOffer3Key
                , currentUserOffer4Key
                ]
          in do time <- liftIO getCurrentTime
                trade1Key <- Db.createTrade currentUserOffer2Key user4OfferKey False time
                trade2Key <- Db.createTrade currentUserOffer3Key user2OfferKey False time
                mbOffer <- (Sql.Entity user4OfferKey <$>) <$> Db.run (Sql.get user4OfferKey)
                mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
                let mbUserOfferEnts = zipWith Sql.Entity userOffers <$> mbUserOffers
                acceptedOfferTrade <-
                  join <$> sequence (liftA2 findAcceptedTrade mbOffer mbUserOfferEnts)
                return (Sql.entityKey <$> acceptedOfferTrade, trade1Key)
        acceptedTradeKey `shouldBe` Just expected
    context "getMatches" $ do
      it "can get all users for offers within a given radius" $ \config ->
        let expectedExchangeOfferDescriptions =
              [["wooden battleship", "wooden rowboat", "wooden canoe"], ["some painting"]]
        in do matches <-
                Spec.runAppToIO config $ do
                  DbSetup {..} <- dbSetup
                  currentUser <- Db.run (Sql.get currentUserKey)
                  traverse getMatches currentUser
              (((\MatchResponse {..} -> description offer) <$>) <$> matches) `shouldBe`
                Just ["man in rain - watercolor", "water color 30x40 painting"]
              ((distance <$>) <$> matches) `shouldBe` Just [9, 6]
              ((username . user <$>) <$> matches) `shouldBe` Just ["milliebob", "freddyjohn"]
              (((((\ExchangeOffer {..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe`
                Just expectedExchangeOfferDescriptions
      it "excludes offers when they are in trade with current user's matching offer" $ \config -> do
        matches <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            DbSetup {..} <- dbSetup
            currentUser <- Db.run (Sql.get currentUserKey)
            tradeKey <-
              Db.createTrade user2OfferKey currentUserOffer1Key False time
            tradeChatKey <- Db.createTradeChat tradeKey time
            traverse getMatches currentUser
        (((\MatchResponse {..} -> description offer) <$>) <$> matches) `shouldBe`
          Just ["man in rain - watercolor"]
        ((distance <$>) <$> matches) `shouldBe` Just [9]
        (((username . user) <$>) <$> matches) `shouldBe` Just ["milliebob"]
        (((((\ExchangeOffer {..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe`
          Just [["wooden battleship", "wooden rowboat", "wooden canoe"]]
      it "pushes exchange offers to front of match stack (if trade is not mutual)" $ \config ->
        let expectedExchangeOfferDescriptions =
              [["some painting"], ["wooden battleship", "wooden rowboat", "wooden canoe"]]
        in do matches <-
                Spec.runAppToIO config $ do
                  time <- liftIO getCurrentTime
                  DbSetup {..} <- dbSetup
                  currentUser <- Db.run (Sql.get currentUserKey)
                  tradeKey <- Db.createTrade currentUserOffer1Key user2OfferKey False time
                  traverse getMatches currentUser
              (fmap (\MatchResponse {..} -> description offer) <$> matches) `shouldBe`
                Just ["water color 30x40 painting", "man in rain - watercolor"]
              (fmap distance <$> matches) `shouldBe` Just [6, 9]
              (fmap (username . user) <$> matches) `shouldBe` Just ["freddyjohn", "milliebob"]
              (((fmap (\ExchangeOffer {..} -> description offer) . exchangeOffers) <$>) <$> matches) `shouldBe`
                Just expectedExchangeOfferDescriptions
              ((fmap (fmap isAccepted . exchangeOffers)) <$> matches) `shouldBe`
                Just [[True], [False, False, False]]
      it "excludes exchange offers (if trade is mutual)" $ \config ->
        let expectedExchangeOfferDescriptions =
              [["wooden battleship", "wooden rowboat", "wooden canoe"]]
        in do matches <-
                Spec.runAppToIO config $ do
                  time <- liftIO getCurrentTime
                  DbSetup {..} <- dbSetup
                  currentUser <- Db.run (Sql.get currentUserKey)
                  tradeKey <-
                    Db.createTrade
                      currentUserOffer1Key
                      user2OfferKey
                      False
                      time
                  tradeChatKey <- Db.createTradeChat tradeKey time
                  traverse getMatches currentUser
              (((\MatchResponse {..} -> description offer) <$>) <$> matches) `shouldBe`
                Just ["man in rain - watercolor"]
              ((distance <$>) <$> matches) `shouldBe` Just [9]
              ((username . user <$>) <$> matches) `shouldBe` Just ["milliebob"]
              (((((\ExchangeOffer {..} -> description offer) <$>) . exchangeOffers) <$>) <$> matches) `shouldBe`
                Just expectedExchangeOfferDescriptions
      it "does not return duplicate matches" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        (matches, offer4MatchKeys) <-
          Spec.runAppToIO config $ do
            currentUser <- Db.run (Sql.get currentUserKey)
            mbMatches <- traverse getMatches currentUser
            let offer4Matches =
                  (exchangeOffers =<<) .
                  filter (\MatchResponse {..} -> Api.Offer.id offer == Sql.fromSqlKey user4OfferKey) <$>
                  mbMatches
                offer4MatchKeys =
                  traverse
                    ((\ExchangeOffer {..} -> Sql.toSqlKey $ Api.Offer.id offer) <$>)
                    offer4Matches
            return (mbMatches, offer4MatchKeys)
        length <$> matches `shouldBe` Just 2
        offer4MatchKeys `shouldBe` Just <$>
          [currentUserOffer2Key, currentUserOffer3Key, currentUserOffer4Key]
