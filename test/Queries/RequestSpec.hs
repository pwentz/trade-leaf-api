{-# LANGUAGE RecordWildCards #-}

module Queries.RequestSpec where

import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main as Db
import Control.Monad.IO.Class (liftIO)
import SpecHelper (runAppToIO, setupTeardown)
import Models.Request
import Models.Offer
import Models.User
import Models.Photo
import Models.Category
import Queries.Request
import Data.Time (getCurrentTime, UTCTime)
import Test.Hspec
import Test.QuickCheck

defaultUser :: UTCTime -> User
defaultUser time =
  User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time time


spec :: Spec
spec = do
  around setupTeardown $ do
    describe "Queries.Request" $ do
      it "can get the request for a given offer" $ \config -> do
        time <- liftIO getCurrentTime
        reqRes <- runAppToIO config $ do
          userKey <- Db.run $ Pg.insert (defaultUser time)
          photoKey <- Db.run $ Pg.insert (Photo Nothing "dog.png" time time)
          categoryKey <- Db.run $ Pg.insert (Category "tutor" time time)
          offerKey <- Db.run $ Pg.insert (Offer userKey categoryKey photoKey "physics" 1 time time)
          reqKey <- Db.run $ Pg.insert (Request offerKey categoryKey "chemistry" time time)
          getOfferRequest offerKey
        ((requestDescription . Pg.entityVal) <$> reqRes) `shouldBe` Just "chemistry"
