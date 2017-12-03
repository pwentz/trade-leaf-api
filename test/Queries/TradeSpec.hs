module Queries.TradeSpec where

import           Queries.Trade
import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.Trade
import           Models.User
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

defaultUser :: UTCTime -> User
defaultUser time =
  User
    { userFirstName = "Doug"
    , userLastName = "Stamper"
    , userEmail = "dougiestamps@yahoo.com"
    , userUsername = "dstamper2"
    , userPassword = "underwood4prez"
    , userPhotoId = Nothing
    , userCoordinates = Nothing
    , userCreatedAt = time
    , userUpdatedAt = time
    }

spec :: Spec
spec =
  around setupTeardown $
    describe "Queries.Trade" $ do
      it "findFromOffers" $ \config -> do
        (foundTradeKey, existingTradeKey) <- runAppToIO config $ do
          time <- liftIO getCurrentTime
          photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
          categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
          userKey <- Db.run $ Pg.insert (defaultUser time)
          offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
          offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
          tradeKey <- Db.run $ Pg.insert (Trade offer1Key offer2Key True time time)
          foundTrade <- findFromOffers offer2Key offer1Key
          return (Pg.entityKey <$> foundTrade, tradeKey)
        foundTradeKey `shouldBe` Just existingTradeKey
      it "findAccepted" $ \config -> do
        (foundTrades, expectedTrades) <- runAppToIO config $ do
          time <- liftIO getCurrentTime
          photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
          categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
          userKey <- Db.run $ Pg.insert (defaultUser time)
          acceptedOfferKey <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
          exchangeOffer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "calculus" 1 time time)
          exchangeOffer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
          exchangeOffer3Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "biology" 1 time time)
          trade1Key <- Db.run $ Pg.insert (Trade acceptedOfferKey exchangeOffer1Key True time time)
          trade2Key <- Db.run $ Pg.insert (Trade exchangeOffer3Key exchangeOffer2Key True time time)
          trade3Key <- Db.run $ Pg.insert (Trade acceptedOfferKey exchangeOffer3Key True time time)
          foundTrades <- findAccepted acceptedOfferKey
          return (Pg.entityKey <$> foundTrades, [trade1Key, trade3Key])
        foundTrades `shouldBe` expectedTrades
      it "findExchange" $ \config -> do
        (foundTrades, expectedTrades) <- runAppToIO config $ do
          time <- liftIO getCurrentTime
          photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
          categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
          userKey <- Db.run $ Pg.insert (defaultUser time)
          exchangeOfferKey <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
          acceptedOffer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "calculus" 1 time time)
          acceptedOffer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
          acceptedOffer3Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "biology" 1 time time)
          trade1Key <- Db.run $ Pg.insert (Trade acceptedOffer3Key acceptedOffer1Key True time time)
          trade2Key <- Db.run $ Pg.insert (Trade acceptedOffer2Key exchangeOfferKey True time time)
          trade3Key <- Db.run $ Pg.insert (Trade acceptedOffer3Key exchangeOfferKey True time time)
          foundTrades <- findExchange exchangeOfferKey
          return (Pg.entityKey <$> foundTrades, [trade2Key, trade3Key])
        foundTrades `shouldBe` expectedTrades
