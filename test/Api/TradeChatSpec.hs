{-# LANGUAGE FlexibleContexts #-}

module Api.TradeChatSpec where

import           Api.TradeChat
import Api.Error (apiErr, StatusCode(E401), ApiErr(RequestedUserNotAuth))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.TradeChat
import           Models.User
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Api.TradeChat" $ do
      context "createTradeChat" $
        it "creates a new TradeChat" $ \config ->
          let tradeChatReq tradeKey =
                TradeChatRequest {tradeId = Pg.fromSqlKey tradeKey}
          in do (newTradeChat, tradeKey, tradeChatCount) <-
                  Spec.runAppToIO config $ do
                    time <- liftIO getCurrentTime
                    photoKey <- Db.createPhoto "cat.png" time
                    userKey <-
                      Db.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
                    categoryKey <- Db.createCategory "tutor" time
                    offer1Key <- Db.createOffer userKey categoryKey photoKey "chemistry" 1 time
                    offer2Key <- Db.createOffer userKey categoryKey photoKey "history" 1 time
                    tradeKey <- Db.createTrade offer1Key offer2Key False time
                    tradeChatKey <- createTradeChat (tradeChatReq tradeKey)
                    mbTradeChat <- Db.run $ Pg.get (Pg.toSqlKey tradeChatKey :: Pg.Key TradeChat)
                    tradeChatCount <- Db.run $ Pg.count ([] :: [Pg.Filter TradeChat])
                    return (mbTradeChat, tradeKey, tradeChatCount)
                tradeChatCount `shouldBe` 1
                tradeChatTradeId <$> newTradeChat `shouldBe` Just tradeKey
      context "fetchChatData" $
        it "throws if passed a user that does not match " $ \config ->
          let
            currentUser time =
              Db.newUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
          in do
          (randomUserId, authedUser) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.createPhoto "cat.png" time
            userKey <- Db.run $ Pg.insert (currentUser time)
            randomUserKey <-
              Db.createUser "fred" "jones" "fj@gmail.com" "fjones1" "pass" Nothing Nothing time
            return (Pg.fromSqlKey randomUserKey, currentUser time)
          Spec.runAppToIO config (fetchChatData randomUserId authedUser) `shouldThrow` (== apiErr (E401, RequestedUserNotAuth))
