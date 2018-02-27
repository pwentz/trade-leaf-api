{-# LANGUAGE FlexibleContexts #-}

module Api.TradeChatSpec where

import           Api.TradeChat
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
  describe "Api.TradeChat" $
  context "createTradeChat" $
  it "creates a new TradeChat" $ \config ->
    let tradeChatReq tradeKey =
          TradeChatRequest {tradeId = Pg.fromSqlKey tradeKey}
    in do (newTradeChat, tradeKey, tradeChatCount) <-
            Spec.runAppToIO config $ do
              time <- liftIO getCurrentTime
              photoKey <- Spec.createPhoto "cat.png" time
              userKey <-
                Spec.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
              categoryKey <- Spec.createCategory "tutor" time
              offer1Key <- Spec.createOffer userKey categoryKey photoKey "chemistry" 1 time
              offer2Key <- Spec.createOffer userKey categoryKey photoKey "history" 1 time
              tradeKey <- Spec.createTrade offer1Key offer2Key False time
              tradeChatKey <- createTradeChat (tradeChatReq tradeKey)
              mbTradeChat <- Db.run $ Pg.get (Pg.toSqlKey tradeChatKey :: Pg.Key TradeChat)
              tradeChatCount <- Db.run $ Pg.count ([] :: [Pg.Filter TradeChat])
              return (mbTradeChat, tradeKey, tradeChatCount)
          tradeChatCount `shouldBe` 1
          tradeChatTradeId <$> newTradeChat `shouldBe` Just tradeKey
