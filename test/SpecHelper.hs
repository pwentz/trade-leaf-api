module SpecHelper where

import           Config                      (App, Config (..),
                                              Environment (Test), makePool,
                                              runApp)
import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Reader        (runReaderT)
import           Database.Persist.Postgresql (runSqlPool)
import           Database.Persist.Sql        (ConnectionPool, Filter,
                                              deleteWhere)
import           Models

runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
    result <- runExceptT $ runReaderT (runApp app) config
    either throwIO return result

setupTeardown :: (Config -> IO a) -> IO ()
setupTeardown runTestsWith = do
    pool <- makePool Test
    migrateDb pool
    cleanDb pool
    runTestsWith $ Config { getPool = pool
                          , getEnv = Test
                          , getJwtSecret = "trade-leaf-secret" }
    cleanDb pool
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb pool = runSqlPool doMigrations pool
    cleanDb :: ConnectionPool -> IO ()
    cleanDb pool = do
        runSqlPool (deleteWhere ([] :: [Filter Request])) pool
        runSqlPool (deleteWhere ([] :: [Filter Offer])) pool
        runSqlPool (deleteWhere ([] :: [Filter Category])) pool
        runSqlPool (deleteWhere ([] :: [Filter User])) pool
