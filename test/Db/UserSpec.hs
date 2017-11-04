{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Db.UserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.IO.Class

import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Database.Persist.Sql        (get, toSqlKey)
import           Servant

import           Api.User                    (UserLocation (..),
                                              UserRequest (..), createUser)
import           Data.Time                   (getCurrentTime)
import           Models
import           SpecHelper                  (runAppToIO, setupTeardown)

defaultReq :: UserRequest
defaultReq =
    UserRequest "username" "password" "password" (UserLocation 0 0)

spec :: Spec
spec =
    around setupTeardown $ do
        describe "User" $ do
            it "createUser creates a new user from a request" $ \config -> do
                time <- liftIO getCurrentTime
                dbUser <-
                    runAppToIO config $ do
                        userId <- createUser defaultReq
                        mbUser <- runDb $ get (toSqlKey userId :: Key User)
                        return (userUsername <$> mbUser)
                dbUser `shouldBe` (Just "username")
