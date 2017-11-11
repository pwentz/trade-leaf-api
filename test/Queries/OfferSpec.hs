module Queries.OfferSpec where

import Test.Hspec
import Test.QuickCheck
import SpecHelper (runAppToIO, setupTeardown)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime, UTCTime)
import qualified Database.Persist.Postgresql as Sql
import Models
import Config (App)
import Queries.Offer (userOffers)


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
            userKey <- runDb $ Sql.insert (defaultUser time)
            randomUserKey <- runDb $ Sql.insert randomUser
            categoryKey <- runDb $ Sql.insert (Category "something" time time)
            photoKey <- runDb $ Sql.insert (Photo Nothing "some-image.jpeg" time time)
            userOffer1 <- runDb $ Sql.insert (Offer userKey categoryKey photoKey "baby sitter" 1 time time)
            userOffer2 <- runDb $ Sql.insert (Offer userKey categoryKey photoKey "circus clown" 1 time time)
            offer3 <- runDb $ Sql.insert (Offer randomUserKey categoryKey photoKey "carpentry" 1 time time)
            offers <- userOffers userKey
            return ((offerDescription . Sql.entityVal) <$> offers)
        userOfferDescriptions `shouldBe` ["baby sitter", "circus clown"]
