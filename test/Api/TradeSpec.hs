{-# LANGUAGE FlexibleContexts #-}

module Api.TradeSpec where

import           Api.Trade
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
    describe "Api.Trade" $ do
      it "createTrade" $ \config ->
        let
          tradeReq offer1Key offer2Key =
            TradeRequest (Pg.fromSqlKey offer1Key) (Pg.fromSqlKey offer2Key)
          in do
          tradeCount <- runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
            categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
            userKey <- Db.run $ Pg.insert (defaultUser time)
            offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
            offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
            _ <- createTrade (tradeReq offer1Key offer2Key)
            Db.run $ Pg.count ([] :: [Pg.Filter Trade])
          tradeCount `shouldBe` 1
      context "getTrade" $ do
        it "takes both offer ids and gets trade" $ \config -> do
            (trade, offer1Key, offer2Key) <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
              categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
              userKey <- Db.run $ Pg.insert (defaultUser time)
              offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
              offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
              targetTrade <- Db.run $ Pg.insert
                                    Trade
                                      { tradeIsMutual = True
                                      , tradeAcceptedOfferId = offer1Key
                                      , tradeExchangeOfferId = offer2Key
                                      , tradeCreatedAt = time
                                      , tradeUpdatedAt = time
                                      }
              otherTrade <- Db.run $ Pg.insert
                                    Trade
                                      { tradeIsMutual = True
                                      , tradeAcceptedOfferId = offer2Key
                                      , tradeExchangeOfferId = offer1Key
                                      , tradeCreatedAt = time
                                      , tradeUpdatedAt = time
                                      }
              foundTrade <- getTrade (Just $ Pg.fromSqlKey offer1Key) (Just $ Pg.fromSqlKey offer2Key)
              return (foundTrade, offer1Key, offer2Key)
            tradeAcceptedOfferId . Pg.entityVal <$> trade `shouldBe` Just offer1Key
            tradeExchangeOfferId . Pg.entityVal <$> trade `shouldBe` Just offer2Key
        it "returns first matching trade if multiple are available" $ \config -> do
              (trade, offer1Key, offer2Key) <- runAppToIO config $ do
                time <- liftIO getCurrentTime
                photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
                categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
                userKey <- Db.run $ Pg.insert (defaultUser time)
                offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
                offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
                offer3Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "biology" 1 time time)
                firstTrade <- Db.run $ Pg.insert
                                      Trade
                                        { tradeIsMutual = True
                                        , tradeAcceptedOfferId = offer1Key
                                        , tradeExchangeOfferId = offer2Key
                                        , tradeCreatedAt = time
                                        , tradeUpdatedAt = time
                                        }
                nextTrade <- Db.run $ Pg.insert
                                      Trade
                                        { tradeIsMutual = True
                                        , tradeAcceptedOfferId = offer1Key
                                        , tradeExchangeOfferId = offer3Key
                                        , tradeCreatedAt = time
                                        , tradeUpdatedAt = time
                                        }
                foundTrade <- getTrade (Just $ Pg.fromSqlKey offer1Key) Nothing
                return (foundTrade, offer1Key, offer2Key)
              tradeAcceptedOfferId . Pg.entityVal <$> trade `shouldBe` Just offer1Key
              tradeExchangeOfferId . Pg.entityVal <$> trade `shouldBe` Just offer2Key
        it "returns nothing if no trade is available" $ \config -> do
              foundTrade <- runAppToIO config (getTrade (Just 1) (Just 2))
              foundTrade `shouldBe` Nothing
      context "patchTrade" $ do
        it "updates multiple fields on trade" $ \config -> do
            (trade, offer2Key, offer3Key) <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
              categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
              userKey <- Db.run $ Pg.insert (defaultUser time)
              offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
              offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
              offer3Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "biology" 1 time time)
              tradeKey <- Db.run $ Pg.insert
                                    Trade
                                      { tradeIsMutual = True
                                      , tradeAcceptedOfferId = offer1Key
                                      , tradeExchangeOfferId = offer2Key
                                      , tradeCreatedAt = time
                                      , tradeUpdatedAt = time
                                      }
              _ <- patchTrade (Pg.fromSqlKey tradeKey) $
                      TradePatchReq
                        (Just False)
                        (Just (Pg.fromSqlKey offer2Key))
                        (Just (Pg.fromSqlKey offer3Key))
              foundTrade <- Db.run $ Pg.get tradeKey
              return (foundTrade, offer2Key, offer3Key)
            tradeAcceptedOfferId <$> trade `shouldBe` Just offer2Key
            tradeExchangeOfferId <$> trade `shouldBe` Just offer3Key
            tradeIsMutual <$> trade `shouldBe` Just False
        it "only updates values that are present on patch request" $ \config -> do
            (trade, offer1Key, offer3Key) <- runAppToIO config $ do
              time <- liftIO getCurrentTime
              photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
              categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
              userKey <- Db.run $ Pg.insert (defaultUser time)
              offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
              offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
              offer3Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "biology" 1 time time)
              tradeKey <- Db.run $ Pg.insert
                                    Trade
                                      { tradeIsMutual = True
                                      , tradeAcceptedOfferId = offer1Key
                                      , tradeExchangeOfferId = offer2Key
                                      , tradeCreatedAt = time
                                      , tradeUpdatedAt = time
                                      }
              _ <- patchTrade (Pg.fromSqlKey tradeKey) $
                      TradePatchReq
                        (Just False)
                        Nothing
                        (Just (Pg.fromSqlKey offer3Key))
              foundTrade <- Db.run $ Pg.get tradeKey
              return (foundTrade, offer1Key, offer3Key)
            tradeAcceptedOfferId <$> trade `shouldBe` Just offer1Key
            tradeExchangeOfferId <$> trade `shouldBe` Just offer3Key
            tradeIsMutual <$> trade `shouldBe` Just False
