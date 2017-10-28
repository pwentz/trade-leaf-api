{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Db.UserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader        (runReaderT)

import           Database.Persist.Postgresql (Entity (..), deleteWhere,
                                              fromSqlKey, insert, runSqlPool,
                                              selectFirst, selectList, (==.))
import           Database.Persist.Sql        (ConnectionPool, get, toSqlKey,
                                              transactionUndo)
import           Database.Persist.Types      (Filter)
import           Servant

import           Api.User                    (UserRequest (..), createUser)
import           Config                      (App, Config (..),
                                              Environment (..), makePool,
                                              runApp)
import           Data.Time                   (getCurrentTime)
import           Models

runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
    result <- runExceptT $ runReaderT (runApp app) config
    case result of
        Left err -> throwIO err
        Right a  -> return a

setupTeardown :: (Config -> IO a) -> IO ()
setupTeardown runTestsWith = do
    pool <- makePool Test
    cleanDb pool
    migrateDb pool
    runTestsWith $ Config { getPool = pool
                          , getEnv = Test
                          , getJwtSecret = "trade-leaf-secret" }
    return ()
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb pool = runSqlPool doMigrations pool
    cleanDb :: ConnectionPool -> IO ()
    cleanDb = deleteAllUsers
    deleteAllUsers :: ConnectionPool -> IO ()
    deleteAllUsers pool = do
        runSqlPool (deleteWhere ([] :: [Filter User])) pool

defaultReq :: UserRequest
defaultReq =
  UserRequest "username" "password" "password" (0, 0) Nothing

-- for more detail, see `src/Config.hs`, but this assumes you have...
--   1. a Postgres `test` user
--   2. a `perservant-test` DB
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
