{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Db.UserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.IO.Class
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Database.Persist.Sql        (fromSqlKey, get, toSqlKey)
import           Servant

import           Api.User                    (PhotoRequest (..),
                                              UserLocation (..),
                                              UserRequest (..), createPhoto,
                                              createUser, getUser, updateCoords)
import           Data.Coords                 (toCoords)
import           Data.Time                   (UTCTime, getCurrentTime)
import           Models
import           SpecHelper                  (runAppToIO, setupTeardown)

defaultReq :: UserRequest
defaultReq = UserRequest "username" "password" "password" Nothing Nothing

spec :: Spec
spec =
    around setupTeardown $ do
        describe "User" $ do
            it "createUser creates a new user from a request" $ \config -> do
                time <- liftIO getCurrentTime
                dbUser <-
                    runAppToIO config $ do
                        userId <- createUser defaultReq
                        mbUser <- getUser userId
                        return (userUsername <$> mbUser)
                dbUser `shouldBe` (Just "username")
            it "updates a user's location" $ \config -> do
                time <- liftIO getCurrentTime
                userCoords <-
                    runAppToIO config $ do
                        userId <- createUser defaultReq
                        mbUser <- getUser userId
                        traverse (updateCoords userId (UserLocation 12.34 56.789)) mbUser
                        updatedUser <- getUser userId
                        return (userCoordinates =<< updatedUser)
                userCoords `shouldBe` (Just $ toCoords 12.34 56.789)
            it "create a photo" $ \config -> do
                time <- liftIO getCurrentTime
                photoUrl <-
                  runAppToIO config $ do
                    photoId <- createPhoto (PhotoRequest Nothing "https://google.com/images/clown")
                    mbPhoto <- runDb $ get (toSqlKey photoId)
                    return (photoImageUrl <$> mbPhoto)
                photoUrl `shouldBe` (Just "https://google.com/images/clown")
