{-# LANGUAGE FlexibleContexts #-}

module Api.RequestSpec where

import           Api.Request
import Config (App)
import Api.Error
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
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck
import Data.Maybe (fromJust)

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Api.Request" $ do
      context "toRequestResponse" $
        it "converts a Request entity to a RequestResponse" $ \config ->
          let expectedReq reqKey offerKey =
                RequestResponse
                 (Pg.fromSqlKey reqKey)
                 (Pg.fromSqlKey offerKey)
                 "tutor"
                 "physics"
          in do (reqRes, expected) <-
                  Spec.runAppToIO config $ do
                    time <- liftIO getCurrentTime
                    photoKey <- Db.createPhoto "dog.png" time
                    userKey <-
                      Db.createUser "ned" "flanders" "n@gmail" "nflan" "password" Nothing Nothing time
                    categoryKey <- Db.createCategory "tutor" time
                    offerKey <- Db.createOffer userKey categoryKey photoKey "chemistry" 1 time
                    reqKey <- Db.createRequest offerKey categoryKey "physics" time
                    mbSampleReq <- fmap (Pg.Entity reqKey) <$> Db.run (Pg.get reqKey)
                    reqResponse <- join <$> traverse toRequestResponse mbSampleReq
                    return (reqResponse, expectedReq reqKey offerKey)
                reqRes `shouldBe` Just expected
      context "createRequest" $ do
        it "creates a request" $ \config -> do
          createRequest <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.createPhoto "dog.png" time
            userKey <-
              Db.createUser "ned" "flanders" "n@gmail" "nflan" "password" Nothing Nothing time
            currentUser <- fromJust <$> Db.run (Pg.get userKey) :: App User
            categoryKey <- Db.createCategory "tutor" time
            offerKey <- Db.createOffer userKey categoryKey photoKey "chemistry" 1 time
            reqId <-
              createRequest
                (RequestRequest (Pg.fromSqlKey offerKey) (Pg.fromSqlKey categoryKey) "looking for a tutor!")
                currentUser
            Db.run (Pg.get $ Pg.toSqlKey reqId) :: App (Maybe Request)
          requestDescription <$> createRequest `shouldBe` Just "looking for a tutor!"
        it "throws if given an offerId that does not belong to the given user" $ \config -> do
          (reqReq, currentUser) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.createPhoto "dog.png" time
            userKey <-
              Db.createUser "ned" "flanders" "n@gmail" "nflan" "password" Nothing Nothing time
            currentUser <- fromJust <$> Db.run (Pg.get userKey) :: App User
            randomUserKey <-
              Db.createUser "jed" "flanders" "j@gmail" "jflan" "password" Nothing Nothing time
            categoryKey <- Db.createCategory "tutor" time
            offerKey <- Db.createOffer randomUserKey categoryKey photoKey "chemistry" 1 time
            return
              (RequestRequest (Pg.fromSqlKey offerKey) (Pg.fromSqlKey categoryKey) "looking for a tutor!"
              , currentUser
              )
          Spec.runAppToIO config (createRequest reqReq currentUser) `shouldThrow` (== apiErr (E401, Unauthorized))
