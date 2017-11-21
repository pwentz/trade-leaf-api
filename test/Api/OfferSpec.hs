{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Api.OfferSpec where

import           Api.Offer                   (OfferResponse (..),
                                              RequestResponse (..),
                                              getOffers,
                                              toOfferResponse, toRequestResponse)
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

data DbSetup = DbSetup
    { userKey :: Pg.Key User
    , offer1Key :: Pg.Key Offer
    , offer2Key :: Pg.Key Offer
    , req1Key :: Pg.Key Request
    , req2Key :: Pg.Key Request
    }

dbSetup :: App DbSetup
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
    req1Key <- Db.run $ Pg.insert (Request offer1Key handyCatKey "sand fence plz" time time)
    offer2Key <- Db.run $ Pg.insert (Offer user1Key babySitterCatKey photo2Key "i sit baby" 10 time time)
    req2Key <- Db.run $ Pg.insert (Request offer2Key tutorCatKey "plz tutor me" time time)
    return $
      DbSetup
        { userKey = user1Key
        , offer1Key = offer1Key
        , offer2Key = offer2Key
        , req1Key = req1Key
        , req2Key = req2Key
        }

offer1Res :: Pg.Key User -> Pg.Key Offer -> Pg.Key Request -> OfferResponse
offer1Res userKey offerKey reqKey =
    OfferResponse
      (Pg.fromSqlKey offerKey)
      (Pg.fromSqlKey userKey)
      "painting"
      "decorative art"
      (RequestResponse (Pg.fromSqlKey reqKey) (Pg.fromSqlKey offerKey) "handywork" "sand fence plz")
      (PhotoRequest Nothing "https://dog.png")

offer2Res :: Pg.Key User -> Pg.Key Offer -> Pg.Key Request -> OfferResponse
offer2Res userKey offerKey reqKey =
    OfferResponse
      (Pg.fromSqlKey offerKey)
      (Pg.fromSqlKey userKey)
      "i sit baby"
      "baby sitter"
      (RequestResponse (Pg.fromSqlKey reqKey) (Pg.fromSqlKey offerKey) "physics tutor" "plz tutor me")
      (PhotoRequest Nothing "https://cat.png")

spec :: Spec
spec = do
  around setupTeardown $ do
    describe "Api.Offer" $ do
      it "can convert an offer into an offer response" $ \config -> do
        DbSetup {..} <- runAppToIO config dbSetup
        offerRes <- runAppToIO config $ do
          mbOffer <- Db.run (Pg.get offer1Key)
          join <$> traverse toOfferResponse ((Pg.Entity offer1Key) <$> mbOffer)
        offerRes `shouldBe` Just (offer1Res userKey offer1Key req1Key)
      it "can convert a request into a requestResponse" $ \config ->
        let
          expectedReq reqKey offerKey =
            RequestResponse
              (Pg.fromSqlKey reqKey)
              (Pg.fromSqlKey offerKey)
              "food"
              "scrub floors"
          sampleReq offerKey catKey time =
            Request offerKey catKey "scrub floors" time time
        in do
        time <- liftIO getCurrentTime
        DbSetup {..} <- runAppToIO config dbSetup
        photoKey <- runAppToIO config (Db.run $ Pg.insert (Photo Nothing "https://cat.png" time time))
        categoryKey <- runAppToIO config (Db.run $ Pg.insert (Category "food" time time))
        offerKey <- runAppToIO config (Db.run $ Pg.insert (Offer userKey categoryKey photoKey "free foodz" 0 time time))
        reqKey <- runAppToIO config (Db.run $ Pg.insert (sampleReq offerKey categoryKey time))
        reqRes <-
          runAppToIO config $
            toRequestResponse (Pg.Entity reqKey (sampleReq offerKey categoryKey time))
        reqRes `shouldBe` Just (expectedReq reqKey offerKey)
      it "can get offer response data for a given user" $ \config -> do
        DbSetup {..} <- runAppToIO config dbSetup
        offers <- runAppToIO config (getOffers (Pg.fromSqlKey userKey))
        offers `shouldBe` [offer1Res userKey offer1Key req1Key , offer2Res userKey offer2Key req2Key]
