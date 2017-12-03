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
            TradeRequest
              { acceptedOfferId = Pg.fromSqlKey offer1Key
              , exchangeOfferId = Pg.fromSqlKey offer2Key
              }
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
      it "makeMutual" $ \config -> do
        mbTrade <- runAppToIO config $ do
          time <- liftIO getCurrentTime
          photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
          categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
          userKey <- Db.run $ Pg.insert (defaultUser time)
          offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
          offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
          tradeKey <- Db.run $ Pg.insert (Trade offer1Key offer2Key True time time)
          _ <- makeMutual (Pg.fromSqlKey tradeKey)
          Db.run (Pg.get tradeKey)
        (tradeIsMutual <$> mbTrade) `shouldBe` Just False
      context "closeOrCreate" $ do
        it "creates a trade if one does not already exist" $ \config ->
          let
            tradeReq offer1Key offer2Key =
              TradeRequest
                { acceptedOfferId = Pg.fromSqlKey offer1Key
                , exchangeOfferId = Pg.fromSqlKey offer2Key
                }
          in do
          tradeCount <- runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
            categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
            userKey <- Db.run $ Pg.insert (defaultUser time)
            offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
            offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
            _ <- closeOrCreate (tradeReq offer1Key offer2Key)
            Db.run $ Pg.count ([] :: [Pg.Filter Trade])
          tradeCount `shouldBe` 1
        it "closes the trade if one already exists with given offers" $ \config ->
          let
            tradeReq offer1Key offer2Key =
              TradeRequest
                { acceptedOfferId = Pg.fromSqlKey offer1Key
                , exchangeOfferId = Pg.fromSqlKey offer2Key
                }
          in do
          (trade, tradeCount) <- runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
            categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
            userKey <- Db.run $ Pg.insert (defaultUser time)
            offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
            offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
            tradeKey <- Db.run $ Pg.insert (Trade offer1Key offer2Key True time time)
            _ <- closeOrCreate (tradeReq offer1Key offer2Key)
            mbTrade <- Db.run $ Pg.get tradeKey
            tradeCount <- Db.run $ Pg.count ([] :: [Pg.Filter Trade])
            return (mbTrade, tradeCount)
          (tradeIsMutual <$> trade) `shouldBe` Just False
          tradeCount `shouldBe` 1
