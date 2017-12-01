{-# LANGUAGE FlexibleContexts #-}

module Api.RequestSpec where

import           Api.Request
import           Control.Monad               (join)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
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
    describe "Api.Request" $
      it "toRequestResponse" $ \config ->
        let
          expectedReq reqKey offerKey =
            RequestResponse
              { Api.Request.id = Pg.fromSqlKey reqKey
              , offerId = Pg.fromSqlKey offerKey
              , category = "tutor"
              , description = "chemistry"
              }
        in do
        (reqRes, expected) <- runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.run $ Pg.insert (Photo Nothing "dog.png" time time)
            userKey <- Db.run $ Pg.insert (defaultUser time)
            categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
            offerKey <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
            reqKey <- Db.run $ Pg.insert (Request offerKey categoryKey "chemistry" time time)
            mbSampleReq <- (Pg.Entity reqKey <$>) <$> Db.run (Pg.get reqKey)
            reqResponse <- join <$> traverse toRequestResponse mbSampleReq
            return (reqResponse, expectedReq reqKey offerKey)
        reqRes `shouldBe` Just expected
