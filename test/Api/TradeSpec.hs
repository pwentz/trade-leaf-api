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
              { offer1Id = Pg.fromSqlKey offer1Key
              , offer2Id = Pg.fromSqlKey offer2Key
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
      it "closeTrade" $ \config -> do
        mbTrade <- runAppToIO config $ do
          time <- liftIO getCurrentTime
          photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
          categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
          userKey <- Db.run $ Pg.insert (defaultUser time)
          offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
          offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chem" 1 time time)
          tradeKey <- Db.run $ Pg.insert (Trade offer1Key offer2Key True time time)
          _ <- closeTrade (Pg.fromSqlKey tradeKey)
          Db.run (Pg.get tradeKey)
        (tradeIsOpen <$> mbTrade) `shouldBe` Just False
