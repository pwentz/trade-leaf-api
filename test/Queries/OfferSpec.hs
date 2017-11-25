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
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck
import           Utils                       (second)


defaultUser :: UTCTime -> User
defaultUser time =
  User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time time


spec :: Spec
spec = do
  around setupTeardown $ do
    describe "Queries.Offer" $ do
      it "can get all offers for a given user" $ \config -> do
        time <- liftIO getCurrentTime
        userOfferDescriptions <-
            let
              randomUser =
                User "fred" "johnson" "fred@gmail.com" "freddy-j" "password" Nothing Nothing time time
            in
            runAppToIO config $ do
            userKey <- Db.run $ Sql.insert (defaultUser time)
            randomUserKey <- Db.run $ Sql.insert randomUser
            categoryKey <- Db.run $ Sql.insert (Category "something" time time)
            photoKey <- Db.run $ Sql.insert (Photo Nothing "some-image.jpeg" time time)
            userOffer1 <- Db.run $ Sql.insert (Offer userKey categoryKey photoKey "baby sitter" 1 time time)
            userOffer2 <- Db.run $ Sql.insert (Offer userKey categoryKey photoKey "circus clown" 1 time time)
            offer3 <- Db.run $ Sql.insert (Offer randomUserKey categoryKey photoKey "carpentry" 1 time time)
            offers <- userOffers userKey
            return ((offerDescription . Sql.entityVal) <$> offers)
        userOfferDescriptions `shouldBe` ["baby sitter", "circus clown"]
      it "can get the request, category, and photo for a given offer" $ \config -> do
        time <- liftIO getCurrentTime
        (user, category, photo) <-
          let
            offer userKey catKey photoKey =
              Offer userKey catKey photoKey "baby sitter" 1 time time
          in
          runAppToIO config $ do
            userKey <- Db.run $ Sql.insert (defaultUser time)
            categoryKey <- Db.run $ Sql.insert (Category "baby sitter" time time)
            otherCategoryKey <- Db.run $ Sql.insert (Category "wood working" time time)
            photoKey <- Db.run $ Sql.insert (Photo Nothing "some-image.jpeg" time time)
            offerKey <- Db.run $ Sql.insert (offer userKey categoryKey photoKey)
            offerReq <- Db.run $ Sql.insert (Request offerKey otherCategoryKey "some request" time time)
            getOfferData (Sql.Entity offerKey (offer userKey categoryKey photoKey))
        (userUsername $ Sql.entityVal user) `shouldBe` "pwentz"
        (E.unValue category) `shouldBe` "baby sitter"
        (photoImageUrl $ Sql.entityVal photo) `shouldBe` "some-image.jpeg"
