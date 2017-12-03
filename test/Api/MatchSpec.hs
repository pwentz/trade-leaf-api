{-# LANGUAGE RecordWildCards #-}

module Api.MatchSpec where

import           Api.Match
import           Api.User                    (UserMeta (username))
import           Api.Offer                   (OfferResponse(description))
import Control.Applicative (liftA2)
import           Config                      (App)
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
import Models.Trade
import           Models.User
import           Queries.Match
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

data DbSetup = DbSetup
    { currentUser :: User
    , currentUserOffer1Key :: Sql.Key Offer
    , currentUserOffer2Key :: Sql.Key Offer
    , currentUserOffer3Key :: Sql.Key Offer
    , matchingOffer1Key :: Sql.Key Offer
    , matchingOffer2Key :: Sql.Key Offer
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

spec :: Spec
spec =
  around setupTeardown $
    describe "Api.Matches" $ do
      context "hasAcceptedOffer" $ do
        it "returns true if given offer is in trade with any given offers" $ \config -> do
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
            sequence (liftA2 hasAcceptedOffer mbOffer mbUserOfferEnts)
          hasApproved `shouldBe` Just True
        it "returns false if given offer is NOT in trade with any given offers" $ \config -> do
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
            sequence (liftA2 hasAcceptedOffer mbOffer mbUserOfferEnts)
          hasApproved `shouldBe` Just False

      context "getMatches" $ do
        it "can get all users for offers within a given radius" $ \config -> do
          matches <- runAppToIO config $ do
              DbSetup {..} <- dbSetup
              getMatches currentUser
          ((description . offer) <$> matches) `shouldContain` ["i sand fence", "it is abstract"]
          (distance <$> matches) `shouldBe` [6, 9]
          ((username . user) <$> matches) `shouldBe` ["ottop", "philQ"]
          (((description <$>) . exchangeOffers) <$> matches) `shouldContain` [["i offer painting"], ["i will sit baby"]]
        it "excludes offers when they are in trade with current user's matching offer" $ \config -> do
          matches <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              DbSetup {..} <- dbSetup
              tradeKey <- Db.run $ Sql.insert (Trade matchingOffer1Key currentUserOffer1Key True time time)
              getMatches currentUser
          ((description . offer) <$> matches) `shouldBe` ["it is abstract"]
          (distance <$> matches) `shouldBe` [9]
          ((username . user) <$> matches) `shouldBe` ["philQ"]
          (((description <$>) . exchangeOffers) <$> matches) `shouldContain` [["i will sit baby"]]
