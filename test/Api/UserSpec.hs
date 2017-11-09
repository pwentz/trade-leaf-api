{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.UserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad               (join)
import           Control.Monad.IO.Class
import Data.Int (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Database.Persist.Sql        (fromSqlKey, get, toSqlKey)
import           Servant

import           Api.Photo                   (PhotoRequest (..), createPhoto)
import           Api.User                    (UserPatchRequest (UserPatchRequest),
                                              UserRequest (UserRequest),
                                              UserMeta(..), createUser,
                                              getUser, patchUser, getUserMeta)
import           Data.Coords                 (Coords(Coords))
import           Data.Time                   (UTCTime, getCurrentTime)
import           Models
import           SpecHelper                  (runAppToIO, setupTeardown)

defaultReq :: UserRequest
defaultReq = UserRequest "username" "password" "password" Nothing Nothing

reqWithData :: Int64 -> UserRequest
reqWithData photoId =
  UserRequest "pat" "password" "password" (Just (Coords 12.345 54.321)) (Just photoId)

photoReq :: PhotoRequest
photoReq = PhotoRequest Nothing "https://google.com/clown.png"

spec :: Spec
spec =
    around setupTeardown $ do
        describe "User" $ do
            it "createUser creates a new user from a request" $ \config -> do
                dbUser <-
                    runAppToIO config $ do
                        userId <- createUser defaultReq
                        mbUser <- getUser userId
                        return (userUsername <$> mbUser)
                dbUser `shouldBe` (Just "username")
            it "updates a user's photo and coordinates" $ \config -> do
                (userCoords, userPhotoImageUrl) <-
                    runAppToIO config $ do
                        userId <- createUser defaultReq
                        photoId <- createPhoto photoReq
                        mbUser <- getUser userId
                        traverse (patchUser userId (UserPatchRequest Nothing (Just photoId) (Just (Coords 12.34 56.789)))) mbUser
                        updatedUser <- getUser userId
                        userPhoto <- join <$> traverse (runDb . get) (userPhotoId =<< updatedUser)
                        return (userCoordinates =<< updatedUser , photoImageUrl <$> userPhoto)
                userCoords `shouldBe` (Just $ Coords 12.34 56.789)
                userPhotoImageUrl `shouldBe` (Just "https://google.com/clown.png")
            it "gets a user's data with records related to user fields" $ \config -> do
              (uname, uphoto, ucoords) <-
                runAppToIO config $ do
                  photoId <- createPhoto photoReq
                  userId <- createUser (reqWithData photoId)
                  userMeta <- getUserMeta userId
                  return (username userMeta, photoImageUrl <$> (photo userMeta), coordinates userMeta)
              uname `shouldBe` "pat"
              uphoto `shouldBe` (Just "https://google.com/clown.png")
              ucoords `shouldBe` (Just $ Coords 12.345 54.321)
