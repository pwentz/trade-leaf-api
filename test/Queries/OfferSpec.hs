module Queries.OfferSpec where

import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Esqueleto          as E
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.User
import           Queries.Offer               (getOfferData, userOffers)
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck
import           Utils                       (first, second, third)

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "Queries.Offer" $ do
    it "can get all offers for a given user" $ \config -> do
      userOfferDescriptions <-
        Spec.runAppToIO config $ do
          time <- liftIO getCurrentTime
          userKey <-
            Db.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time
          randomUserKey <-
            Db.createUser
              "fred"
              "johnson"
              "fred@gmail.com"
              "freddy-j"
              "password"
              Nothing
              Nothing
              time
          categoryKey <- Db.createCategory "something" time
          photoKey <- Db.createPhoto "some-image.jpeg" time
          _ <- Db.createOffer userKey categoryKey photoKey "baby sitter" 1 time
          _ <- Db.createOffer userKey categoryKey photoKey "circus clown" 1 time
          _ <- Db.createOffer randomUserKey categoryKey photoKey "carpentry" 1 time
          fmap (offerDescription . Sql.entityVal) <$> userOffers userKey
      userOfferDescriptions `shouldBe` ["baby sitter", "circus clown"]
    it "can get the request, category, and photo for a given offer" $ \config -> do
      res <-
        Spec.runAppToIO config $ do
          time <- liftIO getCurrentTime
          userKey <-
            Db.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time
          categoryKey <- Db.createCategory "baby sitter" time
          otherCategoryKey <- Db.createCategory "wood working" time
          photoKey <- Db.createPhoto "some-image.jpeg" time
          offerKey <- Db.createOffer userKey categoryKey photoKey "i sit babies" 1 time
          offerReq <- Db.createRequest offerKey otherCategoryKey "some request" time
          mbOffer <- fmap (Sql.Entity offerKey) <$> Db.run (Sql.get offerKey)
          traverse getOfferData mbOffer
      userUsername . Sql.entityVal . first <$> res `shouldBe` Just "pwentz"
      E.unValue . second <$> res `shouldBe` Just "baby sitter"
      photoImageUrl . Sql.entityVal . third <$> res `shouldBe` Just "some-image.jpeg"
