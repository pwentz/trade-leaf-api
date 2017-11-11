{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.UserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad               (join)
import           Control.Monad.IO.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Database.Persist.Sql        (fromSqlKey, get, toSqlKey)
import           Servant

import           Api.Error                   (ApiErr (..))
import           Api.Photo                   (PhotoRequest (..), createPhoto)
import           Api.User                    (UserMeta (..), UserPatchRequest (UserPatchRequest),
                                              UserRequest (UserRequest),
                                              createUser, getUser, getUserMeta,
                                              patchUser)
import           Data.Coords                 (Coords (Coords))
import           Data.Time                   (UTCTime, getCurrentTime)
import           Models
import           SpecHelper                  (runAppToIO, setupTeardown)

defaultReq :: UserRequest
defaultReq =
    UserRequest
        "pat"
        "wentz"
        "pat@gmail.com"
        "username"
        "password"
        Nothing
        Nothing

reqWithData :: Int64 -> UserRequest
reqWithData photoId =
    UserRequest
        "pat"
        "wentz"
        "pat@gmail.com"
        "pwentz"
        "password"
        (Just photoId)
        (Just (Coords 12.345 54.321))

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
            it "does not create user if password is too short" $ \config ->
                  let
                    badReq =
                      UserRequest "pat" "wentz" "pat@gmail.com" "pwentz" "pass" Nothing Nothing
                  in do
                    runAppToIO config (createUser badReq) `shouldThrow` anyException
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
                    in runAppToIO config $ do
                           userId <- createUser defaultReq
                           photoId <- createPhoto photoReq
                           mbUser <- getUser userId
                           traverse (patchUser userId (patchReq photoId)) mbUser
                           updatedUser <- getUser userId
                           userPhoto <- join <$> traverse (runDb . get) (userPhotoId =<< updatedUser)
                           return (updatedUser, userPhoto)
                (userFirstName <$> patchedUser) `shouldBe` (Just "not pat")
                (userLastName <$> patchedUser) `shouldBe` (Just "not wentz")
                (userEmail <$> patchedUser) `shouldBe` (Just "pwentz@yahoo.com")
                (userUsername <$> patchedUser) `shouldBe` (Just "username")
                (userCoordinates =<< patchedUser) `shouldBe` (Just $ Coords 12.34 56.789)
                (photoImageUrl <$> photo) `shouldBe` (Just "https://google.com/clown.png")
            it "gets a user's data with records related to user fields" $ \config -> do
                (uname, uphoto, ucoords) <-
                    runAppToIO config $ do
                        photoId <- createPhoto photoReq
                        userId <- createUser (reqWithData photoId)
                        userMeta <- getUserMeta userId
                        return
                            ( username userMeta
                            , photoImageUrl <$> (photo userMeta)
                            , coordinates userMeta)
                uname `shouldBe` "pwentz"
                uphoto `shouldBe` (Just "https://google.com/clown.png")
                ucoords `shouldBe` (Just $ Coords 12.345 54.321)
