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
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

defaultUser :: UTCTime -> User
defaultUser time =
  User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time time

spec :: Spec
spec =
  around setupTeardown $
    describe "Api.TradeChat" $
      context "createTradeChat" $
        it "creates a new TradeChat" $ \config ->
          let tradeChatReq offer1Key offer2Key =
                TradeChatRequest
                { offer1Id = Pg.fromSqlKey offer1Key
                , offer2Id = Pg.fromSqlKey offer2Key
                }
          in do tradeChatCount <-
                  runAppToIO config $ do
                    time <- liftIO getCurrentTime
                    photoKey <- Db.run $ Pg.insert (Photo Nothing "cat.png" time time)
                    userKey <- Db.run $ Pg.insert (defaultUser time)
                    categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
                    offer1Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "chemistry" 1 time time)
                    offer2Key <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "history" 1 time time)
                    _ <- createTradeChat (tradeChatReq offer1Key offer2Key)
                    Db.run $ Pg.count ([] :: [Pg.Filter TradeChat])
                tradeChatCount `shouldBe` 1
