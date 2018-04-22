{-# LANGUAGE RecordWildCards #-}

module Queries.TradeSpec where

import           Config                      (App)
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
import           Queries.Trade
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

data DbSetup = DbSetup
  { photoKey    :: Pg.Key Photo
  , categoryKey :: Pg.Key Category
  , userKey     :: Pg.Key User
  , offer1Key   :: Pg.Key Offer
  , offer2Key   :: Pg.Key Offer
  , offer3Key   :: Pg.Key Offer
  , offer4Key   :: Pg.Key Offer
  }

dbSetup :: App DbSetup
dbSetup = do
  time <- liftIO getCurrentTime
  photoKey <- Spec.createPhoto "cat.png" time
  categoryKey <- Spec.createCategory "tutor" time
  userKey <-
    Spec.createUser
      "Doug"
      "Stamper"
      "dougiestamps@yahoo.com"
      "dstamper2"
      "underwood4prez"
      Nothing
      Nothing
      time
  offer1Key <- Spec.createOffer userKey categoryKey photoKey "physics" 1 time
  offer2Key <- Spec.createOffer userKey categoryKey photoKey "chem" 1 time
  offer3Key <- Spec.createOffer userKey categoryKey photoKey "biology" 1 time
  offer4Key <- Spec.createOffer userKey categoryKey photoKey "calculus" 1 time
  return (DbSetup photoKey categoryKey userKey offer1Key offer2Key offer3Key offer4Key)

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "Queries.Trade" $ do
    context "findFromOffers" $ do
      it "finds trade with given accepted and exchange offer" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        (foundTradeKey, existingTradeKey) <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            tradeKey <- Spec.createTrade offer1Key offer2Key False time
            foundTrade <- findFromOffers offer1Key offer2Key
            return (Pg.entityKey <$> foundTrade, tradeKey)
        foundTradeKey `shouldBe` Just existingTradeKey
      it "only finds trade where first offer is accepted and second offer is exchange" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        foundTrade <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            tradeKey <- Spec.createTrade offer1Key offer2Key False time
            findFromOffers offer2Key offer1Key
        foundTrade `shouldBe` Nothing
    context "findAccepted" $
      it "finds all trades where given offer is accepted offer" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        (foundTrades, expectedTrades) <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            trade1Key <- Spec.createTrade offer1Key offer4Key False time
            trade2Key <- Spec.createTrade offer3Key offer2Key False time
            trade3Key <- Spec.createTrade offer1Key offer3Key False time
            foundTrades <- findAccepted offer1Key
            return (Pg.entityKey <$> foundTrades, [trade3Key, trade1Key])
        foundTrades `shouldMatchList` expectedTrades
    context "findExchange" $
      it "finds all trades where given offer is exchange offer" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        (foundTrades, expectedTrades) <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            trade1Key <- Spec.createTrade offer3Key offer4Key False time
            trade2Key <- Spec.createTrade offer2Key offer1Key False time
            trade3Key <- Spec.createTrade offer3Key offer1Key False time
            foundTrades <- findExchange offer1Key
            return (Pg.entityKey <$> foundTrades, [trade2Key, trade3Key])
        foundTrades `shouldMatchList` expectedTrades
