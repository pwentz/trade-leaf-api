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
    let tradeChatReq offer1Key offer2Key =
          TradeChatRequest {offer1Id = Pg.fromSqlKey offer1Key, offer2Id = Pg.fromSqlKey offer2Key}
    in do (newTradeChat, offer1Key, offer2Key, tradeChatCount) <-
            Spec.runAppToIO config $ do
              time <- liftIO getCurrentTime
              photoKey <- Spec.createPhoto "cat.png" time
              userKey <-
                Spec.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
              categoryKey <- Spec.createCategory "tutor" time
              offer1Key <- Spec.createOffer userKey categoryKey photoKey "chemistry" 1 time
              offer2Key <- Spec.createOffer userKey categoryKey photoKey "history" 1 time
              tradeChatKey <- createTradeChat (tradeChatReq offer1Key offer2Key)
              mbTradeChat <- Db.run $ Pg.get (Pg.toSqlKey tradeChatKey :: Pg.Key TradeChat)
              tradeChatCount <- Db.run $ Pg.count ([] :: [Pg.Filter TradeChat])
              return (mbTradeChat, offer1Key, offer2Key, tradeChatCount)
          tradeChatCount `shouldBe` 1
          tradeChatOffer1Id <$> newTradeChat `shouldBe` Just offer1Key
          tradeChatOffer2Id <$> newTradeChat `shouldBe` Just offer2Key
