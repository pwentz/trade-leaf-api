{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Api.TradeSpec where

import           Api.Trade
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
import           Models.TradeChat
import           Models.User
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
  return (DbSetup photoKey categoryKey userKey offer1Key offer2Key offer3Key)

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "Api.Trade" $ do
    it "createTrade" $ \config ->
      let tradeReq offer1Key offer2Key =
            TradeRequest (Pg.fromSqlKey offer1Key) (Pg.fromSqlKey offer2Key)
      in do DbSetup {..} <- Spec.runAppToIO config dbSetup
            (newTrade, tradeCount) <-
              Spec.runAppToIO config $ do
                tradeKey <- createTrade (tradeReq offer1Key offer2Key)
                mbTrade <- Db.run $ Pg.get (Pg.toSqlKey tradeKey :: Pg.Key Trade)
                tradeCount <- Db.run $ Pg.count ([] :: [Pg.Filter Trade])
                return (mbTrade, tradeCount)
            tradeCount `shouldBe` 1
            tradeAcceptedOfferId <$> newTrade `shouldBe` Just offer1Key
            tradeExchangeOfferId <$> newTrade `shouldBe` Just offer2Key
    context "getTrade" $ do
      it "takes both offer ids and gets trade" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        trade <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            targetTrade <- Spec.createTrade offer1Key offer2Key Nothing True time
            otherTrade <- Spec.createTrade offer2Key offer1Key Nothing True time
            foundTrade <- getTrade (Just $ Pg.fromSqlKey offer1Key) (Just $ Pg.fromSqlKey offer2Key)
            return foundTrade
        tradeAcceptedOfferId . Pg.entityVal <$> trade `shouldBe` Just offer1Key
        tradeExchangeOfferId . Pg.entityVal <$> trade `shouldBe` Just offer2Key
      it "returns first matching trade if multiple are available" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        trade <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            firstTrade <- Spec.createTrade offer1Key offer2Key Nothing True time
            nextTrade <- Spec.createTrade offer1Key offer3Key Nothing True time
            getTrade (Just $ Pg.fromSqlKey offer1Key) Nothing
        tradeAcceptedOfferId . Pg.entityVal <$> trade `shouldBe` Just offer1Key
        tradeExchangeOfferId . Pg.entityVal <$> trade `shouldBe` Just offer2Key
      it "returns nothing if no trade is available" $ \config -> do
        foundTrade <- Spec.runAppToIO config (getTrade (Just 1) (Just 2))
        foundTrade `shouldBe` Nothing
    context "patchTrade" $ do
      it "updates multiple fields on trade" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        (trade, tradeChatKey) <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            tradeChatKey <- Spec.createTradeChat offer1Key offer2Key time
            tradeKey <- Spec.createTrade offer1Key offer2Key Nothing True time
            _ <-
              patchTrade (Pg.fromSqlKey tradeKey) $
              TradePatchReq
                (Just False)
                (Just (Pg.fromSqlKey tradeChatKey))
                (Just (Pg.fromSqlKey offer2Key))
                (Just (Pg.fromSqlKey offer3Key))
            foundTrade <- Db.run $ Pg.get tradeKey
            return (foundTrade, tradeChatKey)
        tradeAcceptedOfferId <$> trade `shouldBe` Just offer2Key
        tradeExchangeOfferId <$> trade `shouldBe` Just offer3Key
        tradeIsSuccessful <$> trade `shouldBe` Just False
        (tradeTradeChatId =<< trade) `shouldBe` Just tradeChatKey
      it "only updates values that are present on patch request" $ \config -> do
        DbSetup {..} <- Spec.runAppToIO config dbSetup
        trade <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            tradeKey <- Spec.createTrade offer1Key offer2Key Nothing True time
            _ <-
              patchTrade (Pg.fromSqlKey tradeKey) $
              TradePatchReq (Just False) Nothing Nothing (Just (Pg.fromSqlKey offer3Key))
            Db.run (Pg.get tradeKey)
        tradeAcceptedOfferId <$> trade `shouldBe` Just offer1Key
        tradeExchangeOfferId <$> trade `shouldBe` Just offer3Key
        tradeIsSuccessful <$> trade `shouldBe` Just False
