module Queries.RequestSpec where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.User
import           Queries.Request
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "Queries.Request" $
  it "can get the request for a given offer" $ \config -> do
    time <- liftIO getCurrentTime
    reqRes <-
      Spec.runAppToIO config $ do
        userKey <-
          Spec.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time
        photoKey <- Spec.createPhoto "dog.png" time
        categoryKey <- Spec.createCategory "tutor" time
        offerKey <- Spec.createOffer userKey categoryKey photoKey "physics" 1 time
        reqKey <- Spec.createRequest offerKey categoryKey "chemistry" time
        getOfferRequest offerKey
    ((requestDescription . Pg.entityVal) <$> reqRes) `shouldBe` Just "chemistry"
