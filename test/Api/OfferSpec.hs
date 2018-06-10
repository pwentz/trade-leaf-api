{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Api.OfferSpec where

import Api.Error
import           Api.Offer
import           Api.Photo                   (PhotoRequest (..), createPhoto)
import           Api.Request                 (RequestResponse (..))
import           Api.User                    (UserRequest (..), createUser)
import           Config                      (App)
import           Control.Monad               (join)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coords                 (Coords (..))
import           Data.Time                   (UTCTime, getCurrentTime)
import Data.Maybe (fromJust)
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

data DbSetup = DbSetup
  { userKey   :: Pg.Key User
  , offer1Key :: Pg.Key Offer
  , offer2Key :: Pg.Key Offer
  , req1Key   :: Pg.Key Request
  , req2Key   :: Pg.Key Request
  , photo1Key :: Pg.Key Photo
  , photo2Key :: Pg.Key Photo
  , handyworkCategoryKey :: Pg.Key Category
  }

dbSetup :: App DbSetup
dbSetup = do
  time <- liftIO getCurrentTime
  photo1Key <- Db.createPhoto "https://dog.png" time
  photo2Key <- Db.createPhoto "https://cat.png" time
  handyCatKey <- Db.createCategory "handywork" time
  artCatKey <- Db.createCategory "decorative art" time
  babySitterCatKey <- Db.createCategory "baby sitter" time
  tutorCatKey <- Db.createCategory "physics tutor" time
  user1Key <- Db.createUser "jared" "johnson" "jj@yahoo.com" "jj1" "pass" Nothing Nothing time
  offer1Key <- Db.createOffer user1Key artCatKey photo1Key "painting" 999 time
  req1Key <- Db.createRequest offer1Key handyCatKey "sand fence plz" time
  offer2Key <- Db.createOffer user1Key babySitterCatKey photo2Key "i sit baby" 10 time
  req2Key <- Db.createRequest offer2Key tutorCatKey "plz tutor me" time
  return
    DbSetup
    { userKey = user1Key
    , offer1Key = offer1Key
    , offer2Key = offer2Key
    , req1Key = req1Key
    , req2Key = req2Key
    , photo1Key = photo1Key
    , photo2Key = photo2Key
    , handyworkCategoryKey = handyCatKey
    }

photo1 :: UTCTime -> Photo
photo1 time = Db.newPhoto "https://dog.png" time

photo2 :: UTCTime -> Photo
photo2 time = Db.newPhoto "https://cat.png" time

expectedOfferRes1 :: Pg.Key User -> Pg.Key Offer -> Pg.Key Photo -> Pg.Key Request -> UTCTime -> OfferResponse
expectedOfferRes1 userKey offerKey photoKey req1Key time =
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

expectedOfferRes2 :: Pg.Key User -> Pg.Key Offer -> Pg.Key Photo -> Pg.Key Request -> UTCTime -> OfferResponse
expectedOfferRes2 userKey offerKey photoKey req2Key time =
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
  around Spec.setupTeardown $
    describe "Api.Offer" $ do
      context "toOfferResponse" $
        it "can convert an offer into an offer response" $ \config -> do
          time <- liftIO getCurrentTime
          DbSetup {..} <- Spec.runAppToIO config dbSetup
          offerRes <-
            Spec.runAppToIO config $ do
              mbOffer <- Db.run (Pg.get offer1Key)
              traverse toOfferResponse (Pg.Entity offer1Key <$> mbOffer)
          offerRes `shouldBe` Just (expectedOfferRes1 userKey offer1Key photo1Key req1Key time)
      context "getOffers" $
        it "can get offer response data for a given user" $ \config -> do
          time <- liftIO getCurrentTime
          DbSetup {..} <- Spec.runAppToIO config dbSetup
          offers <- Spec.runAppToIO config (getOffers (Pg.fromSqlKey userKey))
          offers `shouldBe`
            [ expectedOfferRes1 userKey offer1Key photo1Key req1Key time
            , expectedOfferRes2 userKey offer2Key photo2Key req2Key time
            ]
      context "createOffer" $ do
        it "creates a new offer from an OfferRequest" $ \config -> do
          DbSetup {..} <- Spec.runAppToIO config dbSetup
          createdOffer <- Spec.runAppToIO config $ do
            currentUser <- fromJust <$> Db.run (Pg.get userKey) :: App User
            offerId <-
              createOffer
                (OfferRequest (Pg.fromSqlKey handyworkCategoryKey) (Pg.fromSqlKey photo1Key) "i can paint some fences!" 55)
                currentUser
            Db.run (Pg.get $ Pg.toSqlKey offerId) :: App (Maybe Offer)
          offerDescription <$> createdOffer `shouldBe` Just "i can paint some fences!"
      context "removeOffer" $ do
        it "throws if trying to remove an offer that doesn't belong to authed user" $ \config -> do
          DbSetup {..} <- Spec.runAppToIO config dbSetup
          newUser <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            newUserKey <-
              Db.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            fromJust <$> Db.run (Pg.get newUserKey) :: App User
          Spec.runAppToIO config (removeOffer (Pg.fromSqlKey offer1Key) newUser) `shouldThrow` (== apiErr (E401, Unauthorized))
        it "removes given offer" $ \config -> do
          DbSetup {..} <- Spec.runAppToIO config dbSetup
          offer <- Spec.runAppToIO config $ do
            mbUser <- Db.run (Pg.get userKey)
            traverse (removeOffer $ Pg.fromSqlKey offer1Key) mbUser
            Db.run (Pg.get offer1Key) :: App (Maybe Offer)
          offer `shouldBe` Nothing
