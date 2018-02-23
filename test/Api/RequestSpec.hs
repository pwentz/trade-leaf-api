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
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "Api.Request" $
  it "toRequestResponse" $ \config ->
    let expectedReq reqKey offerKey =
          RequestResponse
          { Api.Request.id = Pg.fromSqlKey reqKey
          , offerId = Pg.fromSqlKey offerKey
          , category = "tutor"
          , description = "physics"
          }
    in do (reqRes, expected) <-
            Spec.runAppToIO config $ do
              time <- liftIO getCurrentTime
              photoKey <- Spec.createPhoto "dog.png" time
              userKey <-
                Spec.createUser "ned" "flanders" "n@gmail" "nflan" "password" Nothing Nothing time
              categoryKey <- Spec.createCategory "tutor" time
              offerKey <- Spec.createOffer userKey categoryKey photoKey "chemistry" 1 time
              reqKey <- Spec.createRequest offerKey categoryKey "physics" time
              mbSampleReq <- fmap (Pg.Entity reqKey) <$> Db.run (Pg.get reqKey)
              reqResponse <- join <$> traverse toRequestResponse mbSampleReq
              return (reqResponse, expectedReq reqKey offerKey)
          reqRes `shouldBe` Just expected
