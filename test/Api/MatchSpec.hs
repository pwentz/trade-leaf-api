{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.MatchSpec where

import           Api.Match
import           Api.Offer                   (OfferResponse (description, id))
import           Api.User                    (UserMeta (username))
import           Config                      (App)
import           Control.Applicative         (liftA2)
import           Control.Monad               (join)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coords                 (Coords (Coords))
import           Data.Time                   (getCurrentTime)
import           Database.Persist            (get)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.Trade
import           Models.User
import           Queries.Match
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

data DbSetup = DbSetup
    { currentUser          :: User
    , currentUserOffer1Key :: Sql.Key Offer
    , currentUserOffer2Key :: Sql.Key Offer
    , currentUserOffer3Key :: Sql.Key Offer
    , matchingOffer1Key    :: Sql.Key Offer
    , matchingOffer2Key    :: Sql.Key Offer
    }

dbSetup :: App DbSetup
dbSetup =
  let
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time time
  in do
    time <- liftIO getCurrentTime
    photoId <- Db.run $ Sql.insert (Photo Nothing "" time time)
    handyCatKey <- Db.run $ Sql.insert (Category "handywork" time time)
    artCatKey <- Db.run $ Sql.insert (Category "decorative art" time time)
    babysitCatKey <- Db.run $ Sql.insert (Category "babysitter" time time)
    currUserKey <- Db.run $ Sql.insert (currentUser time)
    currUserOffer1 <- Db.run $ Sql.insert (Offer currUserKey artCatKey photoId "i offer painting" 999 time time)
    currUserOffer2 <- Db.run $ Sql.insert (Offer currUserKey babysitCatKey photoId "i will sit baby" 15 time time)
    currUserOffer3 <- Db.run $ Sql.insert (Offer currUserKey handyCatKey photoId "i handy" 15 time time)
    _ <- Db.run $ Sql.insert (Request currUserOffer1 handyCatKey "sand fence plz" time time)
    _ <- Db.run $ Sql.insert (Request currUserOffer2 artCatKey "i need painting" time time)
    {-| 6 miles from currentUser -}
    user1Key <- Db.run $ Sql.insert (User "Otto" "porter" "otto@gmail.com" "ottop" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time time)
    user1Offer <- Db.run $ Sql.insert (Offer user1Key handyCatKey photoId "i sand fence" 10 time time)
    _ <- Db.run $ Sql.insert (Request user1Offer artCatKey "i like painting plz" time time)
    {-| 4 miles from currentUser -}
    user2Key <- Db.run $ Sql.insert (User "Tim" "Thomas" "tim@gmail.com" "timmy_two_time" "password" Nothing (Just $ Coords 41.888730 (-87.687969)) time time)
    user2Offer <- Db.run $ Sql.insert (Offer user2Key handyCatKey photoId "i've sanded once" 1 time time)
    _ <- Db.run $ Sql.insert (Request user2Offer artCatKey "i've always wanted a painting" time time)
    {-| 18 miles from currentUser -}
    user3Key <- Db.run $ Sql.insert (User "Theron" "Mitchell" "theron@gmail.com" "the_ron" "password" Nothing (Just $ Coords 41.680753 (-87.698157)) time time)
    user3Offer <- Db.run $ Sql.insert (Offer user3Key artCatKey photoId "warhol" 20 time time)
    _ <- Db.run $ Sql.insert (Request user3Offer babysitCatKey "my baby needs sitting" time time)
    {-| 14 miles from currentUser -}
    user4Key <- Db.run $ Sql.insert (User "Felicia" "James" "felic@yahoo.com" "fjames" "password" Nothing (Just $ Coords 41.734517 (-87.674043)) time time)
    user4Offer <- Db.run $ Sql.insert (Offer user4Key artCatKey photoId "i fingerpainted" 10 time time)
    _ <- Db.run $ Sql.insert (Request user4Offer babysitCatKey "SIT ON MY BABY" time time)
    {-| 9 miles from currentUser -}
    user5Key <- Db.run $ Sql.insert (User "Phil" "Q" "phil@q.com" "philQ" "password" Nothing (Just $ Coords 41.804575 (-87.671359)) time time)
    user5Offer <- Db.run $ Sql.insert (Offer user5Key artCatKey photoId "it is abstract" 10 time time)
    _ <- Db.run $ Sql.insert (Request user5Offer babysitCatKey "watch all the kids" time time)
    return DbSetup
              { currentUser = (currentUser time)
              , currentUserOffer1Key = currUserOffer1
              , currentUserOffer2Key = currUserOffer2
              , currentUserOffer3Key = currUserOffer3
              , matchingOffer1Key = user1Offer
              , matchingOffer2Key = user5Offer
              }

data DbSetup2 = DbSetup2
    { currentUserKey :: Sql.Key User
    , currentUserOffer1Key :: Sql.Key Offer
    , currentUserOffer2Key :: Sql.Key Offer
    , currentUserOffer3Key :: Sql.Key Offer
    , currentUserOffer4Key :: Sql.Key Offer
    , user2Key :: Sql.Key User
    , user2OfferKey :: Sql.Key Offer
    , user3Key :: Sql.Key User
    , user3OfferKey :: Sql.Key Offer
    , user4Key :: Sql.Key User
    , user4OfferKey :: Sql.Key Offer
    }

dbSetup2 :: App DbSetup2
dbSetup2 =
  let
    coords =
      Coords 41.938132 (-87.642753)
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just coords) time time
  in do
    time <- liftIO getCurrentTime
    photoKey <- Db.run $ Sql.insert (Photo Nothing "" time time)
    woodworkingCategoryKey <- Db.run $ Sql.insert (Category "woodworking" time time)
    artCategoryKey <- Db.run $ Sql.insert (Category "decorative art" time time)
    currentUserKey <- Db.run $ Sql.insert (currentUser time)
    currentUserOffer1Key <- Db.run $ Sql.insert (Offer currentUserKey artCategoryKey photoKey "some painting" 999 time time)
    currentUserRequest1Key <- Db.run $ Sql.insert (Request currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time time)
    user2Key <- Db.run $ Sql.insert (User "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" "password" Nothing (Just coords) time time)
    user2OfferKey <- Db.run $ Sql.insert (Offer user2Key artCategoryKey photoKey "water color 30x40 painting" 999 time time)
    user2OfferRequestKey <- Db.run $ Sql.insert (Request user2OfferKey artCategoryKey "animal painting for kid" time time)
    user3Key <- Db.run $ Sql.insert (User "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" "password" Nothing (Just coords) time time)
    user3OfferKey <- Db.run $ Sql.insert (Offer user3Key artCategoryKey photoKey "finger painting dog with lots of colors" 999 time time)
    user3OfferRequestKey <- Db.run $ Sql.insert (Request user3OfferKey artCategoryKey "looking for a large painting" time time)
    user4Key <- Db.run $ Sql.insert (User "Millie" "Rock" "bobby@brown.com" "milliebob" "password" Nothing (Just coords) time time)
    user4OfferKey <- Db.run $ Sql.insert (Offer user4Key artCategoryKey photoKey "man in rain - watercolor" 999 time time)
    user4OfferRequestKey <- Db.run $ Sql.insert (Request user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time time)
    currentUserOffer2Key <- Db.run $ Sql.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time time)
    currentUserRequest2Key <- Db.run $ Sql.insert (Request currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time time)
    currentUserOffer3Key <- Db.run $ Sql.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time time)
    currentUserRequest3Key <- Db.run $ Sql.insert (Request currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time time)
    currentUserOffer4Key <- Db.run $ Sql.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time time)
    currentUserRequest4Key <- Db.run $ Sql.insert (Request currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time time)
    return DbSetup2
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
  around setupTeardown $
    describe "Api.Matches" $ do
      context "containsExchangeOffer" $ do
        it "returns true if given offer is accepted offer in trade with any given offers" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          hasApproved <- runAppToIO config $
            let
              userOffers =
                [currentUserOffer1Key, currentUserOffer2Key, currentUserOffer3Key]
            in do
            time <- liftIO getCurrentTime
            _ <- Db.run $ Sql.insert (Trade matchingOffer1Key currentUserOffer1Key True time time)
            mbOffer <- (Sql.Entity matchingOffer1Key <$>) <$> Db.run (Sql.get matchingOffer1Key)
            mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
            mbUserOfferEnts <- return $ (zipWith Sql.Entity userOffers) <$> mbUserOffers
            sequence (liftA2 containsExchangeOffer mbOffer mbUserOfferEnts)
          hasApproved `shouldBe` Just True
        it "returns false if given offer is NOT accepted offer in trade with any given offers" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          hasApproved <- runAppToIO config $
            let
              userOffers =
                [currentUserOffer1Key, currentUserOffer2Key, currentUserOffer3Key]
            in do
            time <- liftIO getCurrentTime
            _ <- Db.run $ Sql.insert (Trade currentUserOffer1Key matchingOffer1Key True time time)
            _ <- Db.run $ Sql.insert (Trade currentUserOffer2Key currentUserOffer3Key False time time)
            mbOffer <- (Sql.Entity matchingOffer1Key <$>) <$> Db.run (Sql.get matchingOffer1Key)
            mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
            mbUserOfferEnts <- return $ (zipWith Sql.Entity userOffers) <$> mbUserOffers
            sequence (liftA2 containsExchangeOffer mbOffer mbUserOfferEnts)
          hasApproved `shouldBe` Just False

      context "findAcceptedTrade" $ do
        it "finds trade w/ accepted offer within given list of offers where given offer is exchange offer" $ \config -> do
          DbSetup {..} <- runAppToIO config dbSetup
          (acceptedTradeKey, expected) <- runAppToIO config $
            let
              userOffers =
                [currentUserOffer1Key, currentUserOffer2Key, currentUserOffer3Key]
            in do
              time <- liftIO getCurrentTime
              trade1Key <- Db.run $ Sql.insert (Trade currentUserOffer2Key matchingOffer2Key False time time)
              trade2Key <- Db.run $ Sql.insert (Trade currentUserOffer3Key matchingOffer1Key False time time)
              mbOffer <- (Sql.Entity matchingOffer2Key <$>) <$> Db.run (Sql.get matchingOffer2Key)
              mbUserOffers <- sequence <$> (traverse (Db.run . Sql.get) userOffers)
              mbUserOfferEnts <- return $ (zipWith Sql.Entity userOffers) <$> mbUserOffers
              acceptedOfferTrade <- join <$> sequence (liftA2 findAcceptedTrade mbOffer mbUserOfferEnts)
              return (Sql.entityKey <$> acceptedOfferTrade, trade1Key)
          acceptedTradeKey `shouldBe` Just expected

      context "getMatches" $ do
        it "can get all users for offers within a given radius" $ \config -> do
          matches <- runAppToIO config $ do
              DbSetup {..} <- dbSetup
              getMatches currentUser
          ((\MatchResponse{..} -> description offer) <$> matches) `shouldBe` ["it is abstract", "i sand fence"]
          (distance <$> matches) `shouldBe` [9, 6]
          ((username . user) <$> matches) `shouldBe` ["philQ", "ottop"]
          ((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$> matches) `shouldBe` [["i will sit baby"], ["i offer painting"]]
        it "excludes offers when they are in trade with current user's matching offer" $ \config -> do
          matches <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              DbSetup {..} <- dbSetup
              tradeKey <- Db.run $ Sql.insert (Trade matchingOffer1Key currentUserOffer1Key True time time)
              getMatches currentUser
          ((\MatchResponse{..} -> description offer) <$> matches) `shouldBe` ["it is abstract"]
          (distance <$> matches) `shouldBe` [9]
          ((username . user) <$> matches) `shouldBe` ["philQ"]
          ((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$> matches) `shouldBe` [["i will sit baby"]]
        it "pushes exchange offers to front of match stack (if trade is not mutual)" $ \config -> do
          matches <- runAppToIO config $ do
            time <- liftIO getCurrentTime
            DbSetup {..} <- dbSetup
            tradeKey <- Db.run $ Sql.insert (Trade currentUserOffer2Key matchingOffer2Key False time time)
            getMatches currentUser
          ((\MatchResponse{..} -> description offer) <$> matches) `shouldBe` ["it is abstract", "i sand fence"]
          (distance <$> matches) `shouldBe` [9, 6]
          ((username . user) <$> matches) `shouldBe` ["philQ", "ottop"]
          ((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$> matches) `shouldBe` [["i will sit baby"], ["i offer painting"]]
          (((isAccepted <$>) . exchangeOffers) <$> matches) `shouldBe` [[True], [False]]
        it "excludes exchange offers (if trade is mutual)" $ \config -> do
          matches <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              DbSetup {..} <- dbSetup
              tradeKey <- Db.run $ Sql.insert (Trade currentUserOffer1Key matchingOffer1Key True time time)
              getMatches currentUser
          ((\MatchResponse{..} -> description offer) <$> matches) `shouldBe` ["it is abstract"]
          (distance <$> matches) `shouldBe` [9]
          ((username . user) <$> matches) `shouldBe` ["philQ"]
          ((((\ExchangeOffer{..} -> description offer) <$>) . exchangeOffers) <$> matches) `shouldBe` [["i will sit baby"]]
        it "does not return duplicate matches" $ \config -> do
          DbSetup2 {..} <- runAppToIO config dbSetup2
          (matches, offer4MatchKeys) <- runAppToIO config $ do
            currentUser <- Db.run (Sql.get currentUserKey)
            mbMatches <- traverse getMatches currentUser
            offer4Matches <-
              return $ (exchangeOffers =<<) . filter (\MatchResponse{..} -> Api.Offer.id offer == Sql.fromSqlKey user4OfferKey) <$> mbMatches
            offer4MatchKeys <-
              return $ traverse ((\ExchangeOffer{..} -> Sql.toSqlKey $ Api.Offer.id offer) <$>) offer4Matches
            return (mbMatches, offer4MatchKeys)
          length <$> matches `shouldBe` Just 3
          offer4MatchKeys `shouldBe` Just <$> [currentUserOffer2Key, currentUserOffer3Key, currentUserOffer4Key]
