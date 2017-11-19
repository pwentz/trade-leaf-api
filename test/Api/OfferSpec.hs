{-# LANGUAGE FlexibleContexts #-}
module Api.OfferSpec where

import           Api.Offer                   (OfferResponse (..),
                                              RequestResponse (..), getOffers,
                                              toOfferResponse)
import           Api.Photo                   (PhotoRequest (..))
import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import Control.Monad (join)
import           Data.Time                   (getCurrentTime)
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

dbSetup :: App (Pg.Key User, Pg.Key Offer, Pg.Key Offer)
dbSetup =
  let
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time time
  in do
    time <- liftIO getCurrentTime
    photo1Key <- Db.run $ Pg.insert (Photo Nothing "https://dog.png" time time)
    photo2Key <- Db.run $ Pg.insert (Photo Nothing "https://cat.png" time time)
    handyCatKey <- Db.run $ Pg.insert (Category "handywork" time time)
    artCatKey <- Db.run $ Pg.insert (Category "decorative art" time time)
    babySitterCatKey <- Db.run $ Pg.insert (Category "baby sitter" time time)
    tutorCatKey <- Db.run $ Pg.insert (Category "physics tutor" time time)
    user1Key <- Db.run $ Pg.insert (currentUser time)
    offer1Key <- Db.run $ Pg.insert (Offer user1Key artCatKey photo1Key "painting" 999 time time)
    _ <- Db.run $ Pg.insert (Request offer1Key handyCatKey "sand fence plz" time time)
    offer2Key <- Db.run $ Pg.insert (Offer user1Key babySitterCatKey photo2Key "i sit baby" 10 time time)
    _ <- Db.run $ Pg.insert (Request offer2Key tutorCatKey "plz tutor me" time time)
    return (user1Key, offer1Key, offer2Key)

offer1Res :: Pg.Key User -> Pg.Key Offer -> OfferResponse
offer1Res userKey offerKey =
    OfferResponse
      (Pg.fromSqlKey userKey)
      "painting"
      "decorative art"
      (RequestResponse (Pg.fromSqlKey offerKey) "handywork" "sand fence plz")
      (PhotoRequest Nothing "https://dog.png")

offer2Res :: Pg.Key User -> Pg.Key Offer -> OfferResponse
offer2Res userKey offerKey =
    OfferResponse
      (Pg.fromSqlKey userKey)
      "i sit baby"
      "baby sitter"
      (RequestResponse (Pg.fromSqlKey offerKey) "physics tutor" "plz tutor me")
      (PhotoRequest Nothing "https://cat.png")

spec :: Spec
spec = do
  around setupTeardown $ do
    describe "Api.Offer" $ do
      it "can convert an offer into an offer response" $ \config -> do
        (userKey, offer1Key, _) <- runAppToIO config dbSetup
        offerRes <- runAppToIO config $ do
          mbOffer <- Db.run (Pg.get offer1Key)
          join <$> traverse toOfferResponse ((Pg.Entity offer1Key) <$> mbOffer)
        offerRes `shouldBe` Just (offer1Res userKey offer1Key)
      it "can get offer response data for a given user" $ \config -> do
        (userKey, offer1Key, offer2Key) <- runAppToIO config dbSetup
        offers <- runAppToIO config (getOffers (Pg.fromSqlKey userKey))
        offers `shouldBe` [offer1Res userKey offer1Key , offer2Res userKey offer2Key]
