{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Api.OfferSpec where

import           Api.Offer                   (OfferResponse (..), getOffers,
                                              toOfferResponse)
import           Api.Photo                   (PhotoRequest (..), createPhoto)
import           Api.Request                 (RequestResponse (..))
import           Api.User                    (UserRequest (..), createUser)
import           Config                      (App)
import           Control.Monad               (join)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coords                 (Coords (..))
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

data DbSetup = DbSetup
    { userKey   :: Pg.Key User
    , offer1Key :: Pg.Key Offer
    , offer2Key :: Pg.Key Offer
    , req1Key   :: Pg.Key Request
    , req2Key   :: Pg.Key Request
    , photo1Key :: Pg.Key Photo
    , photo2Key :: Pg.Key Photo
    }

dbSetup :: App DbSetup
dbSetup =
  let
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time time
  in do
    time <- liftIO getCurrentTime
    photo1Key <- Db.run $ Pg.insert (photo1 time)
    photo2Key <- Db.run $ Pg.insert (photo2 time)
    handyCatKey <- Db.run $ Pg.insert (Category "handywork" time time)
    artCatKey <- Db.run $ Pg.insert (Category "decorative art" time time)
    babySitterCatKey <- Db.run $ Pg.insert (Category "baby sitter" time time)
    tutorCatKey <- Db.run $ Pg.insert (Category "physics tutor" time time)
    user1Key <- Db.run $ Pg.insert (currentUser time)
    offer1Key <- Db.run $ Pg.insert (Offer user1Key artCatKey photo1Key "painting" 999 time time)
    req1Key <- Db.run $ Pg.insert (Request offer1Key handyCatKey "sand fence plz" time time)
    offer2Key <- Db.run $ Pg.insert (Offer user1Key babySitterCatKey photo2Key "i sit baby" 10 time time)
    req2Key <- Db.run $ Pg.insert (Request offer2Key tutorCatKey "plz tutor me" time time)
    return
      DbSetup
        { userKey = user1Key
        , offer1Key = offer1Key
        , offer2Key = offer2Key
        , req1Key = req1Key
        , req2Key = req2Key
        , photo1Key = photo1Key
        , photo2Key = photo2Key
        }

photo1 :: UTCTime -> Photo
photo1 time =
  Photo Nothing "https://dog.png" time time

photo2 :: UTCTime -> Photo
photo2 time =
  Photo Nothing "https://cat.png" time time

offer1Res :: Pg.Key User -> Pg.Key Offer -> Pg.Key Photo -> Pg.Key Request -> UTCTime -> OfferResponse
offer1Res userKey offerKey photoKey req1Key time =
    OfferResponse
      { id = Pg.fromSqlKey offerKey
      , userId = Pg.fromSqlKey userKey
      , description = "painting"
      , category = "decorative art"
      , request =
          Just
            RequestResponse
              { id = Pg.fromSqlKey req1Key
              , offerId = Pg.fromSqlKey offerKey
              , category = "handywork"
              , description = "sand fence plz"
              }
      , photo = Pg.Entity photoKey (photo1 time)
      }

offer2Res :: Pg.Key User -> Pg.Key Offer -> Pg.Key Photo -> Pg.Key Request -> UTCTime -> OfferResponse
offer2Res userKey offerKey photoKey req2Key time =
    OfferResponse
      { id = Pg.fromSqlKey offerKey
      , userId = Pg.fromSqlKey userKey
      , description = "i sit baby"
      , category = "baby sitter"
      , request =
          Just
            RequestResponse
              { id = Pg.fromSqlKey req2Key
              , offerId = Pg.fromSqlKey offerKey
              , category = "physics tutor"
              , description = "plz tutor me"
              }
      , photo = Pg.Entity photoKey (photo2 time)
      }

spec :: Spec
spec =
  around setupTeardown $
    describe "Api.Offer" $ do
      it "can convert an offer into an offer response" $ \config -> do
        time <- liftIO getCurrentTime
        DbSetup {..} <- runAppToIO config dbSetup
        offerRes <- runAppToIO config $ do
          mbOffer <- Db.run (Pg.get offer1Key)
          traverse toOfferResponse (Pg.Entity offer1Key <$> mbOffer)
        offerRes `shouldBe` Just (offer1Res userKey offer1Key photo1Key req1Key time)
      it "can get offer response data for a given user" $ \config -> do
        time <- liftIO getCurrentTime
        DbSetup {..} <- runAppToIO config dbSetup
        offers <- runAppToIO config (getOffers (Pg.fromSqlKey userKey))
        offers `shouldBe` [offer1Res userKey offer1Key photo1Key req1Key time, offer2Res userKey offer2Key photo2Key req2Key time]
