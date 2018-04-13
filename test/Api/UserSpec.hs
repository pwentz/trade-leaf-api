{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Api.UserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad               (join)
import           Control.Monad.IO.Class
import           Data.Int                    (Int64)
import qualified Database.Persist.Sql        as Sql
import           Servant

import           Api.Error                   (ApiErr (..))
import           Api.Offer                   (OfferResponse (description))
import           Api.Photo                   (PhotoRequest (..), createPhoto)
import           Api.User                    (UserMeta (..), UserPatchRequest (UserPatchRequest),
                                              UserRequest (UserRequest),
                                              createUser, getUserMeta,
                                              patchUser)
import           Data.Coords                 (Coords (Coords))
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.User
import qualified SpecHelper                  as Spec

spec :: Spec
spec =
  around Spec.setupTeardown $
  describe "User" $ do
    context "createUser" $ do
      it "createUser creates a new user from a request" $ \config -> do
        dbUser <-
          Spec.runAppToIO config $ do
            userId <-
              createUser
                (UserRequest "pat" "wentz" "pat@gmail.com" "username" "password" Nothing Nothing)
            (mbUser :: Maybe User) <- Db.run $ Sql.get (Sql.toSqlKey userId)
            return (userUsername <$> mbUser)
        dbUser `shouldBe` Just "username"
      it "does not create user if password is too short" $ \config ->
        let badReq = UserRequest "pat" "wentz" "pat@gmail.com" "pwentz" "pass" Nothing Nothing
        in Spec.runAppToIO config (createUser badReq) `shouldThrow` anyException
    context "patchUser" $
      it "updates a user's photo and coordinates" $ \config -> do
        (patchedUser, photo) <-
          let patchReq photoId =
                UserPatchRequest
                  (Just "not pat")
                  (Just "not wentz")
                  (Just "pwentz@yahoo.com")
                  Nothing
                  (Just photoId)
                  (Just (Coords 12.34 56.789))
          in Spec.runAppToIO config $ do
               time <- liftIO getCurrentTime
               userKey <-
                 Spec.createUser
                   "pat"
                   "wentz"
                   "pat@gmail.com"
                   "username"
                   "password"
                   Nothing
                   Nothing
                   time
               photoKey <- Spec.createPhoto "https://google.com/clown.png" time
               mbUser <- Db.run (Sql.get userKey)
               traverse
                 (patchUser (Sql.fromSqlKey userKey) (patchReq $ Sql.fromSqlKey photoKey))
                 mbUser
               updatedUser <- Db.run (Sql.get userKey)
               userPhoto <- join <$> traverse (Db.run . Sql.get) (userPhotoId =<< updatedUser)
               return (updatedUser, userPhoto)
        (userFirstName <$> patchedUser) `shouldBe` Just "not pat"
        (userLastName <$> patchedUser) `shouldBe` Just "not wentz"
        (userEmail <$> patchedUser) `shouldBe` Just "pwentz@yahoo.com"
        (userUsername <$> patchedUser) `shouldBe` Just "username"
        (userCoordinates =<< patchedUser) `shouldBe` (Just $ Coords 12.34 56.789)
        (photoImageUrl <$> photo) `shouldBe` Just "https://google.com/clown.png"
    context "getUserMeta" $
      it "gets a user's data with records related to user fields" $ \config -> do
        time <- liftIO getCurrentTime
        userMeta <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Spec.createPhoto "https://google.com/clown.png" time
            userKey <-
              Spec.createUser
                "pat"
                "wentz"
                "pat@gmail.com"
                "pwentz"
                "password"
                (Just photoKey)
                (Just (Coords 12.345 54.321))
                time
            categoryKey <- Spec.createCategory "stuff" time
            userOfferId1 <- Spec.createOffer userKey categoryKey photoKey "babysitting" 5 time
            userOfferId2 <- Spec.createOffer userKey categoryKey photoKey "carpentry" 5 time
            getUserMeta (Sql.fromSqlKey userKey)
        username userMeta `shouldBe` "pwentz"
        ((photoImageUrl . Sql.entityVal) <$> photo userMeta) `shouldBe`
          Just "https://google.com/clown.png"
        coordinates userMeta `shouldBe` (Just $ Coords 12.345 54.321)
        (description <$> offers userMeta) `shouldBe` ["babysitting", "carpentry"]
