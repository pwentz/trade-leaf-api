{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Db.Main where

import           Config
import           Control.Exception    (Exception, SomeException (..),
                                       displayException)
import           Control.Monad.Catch  (catch)
import           Control.Monad.Reader
import           Database.Persist.Sql
import qualified Models.Category      as Category
import qualified Models.Offer         as Offer
import qualified Models.Photo         as Photo
import qualified Models.Request       as Request
import qualified Models.Trade         as Trade
import qualified Models.User          as User

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration Photo.migrateAll
    runMigration Category.migrateAll
    runMigration User.migrateAll
    runMigration Offer.migrateAll
    runMigration Request.migrateAll
    runMigration Trade.migrateAll

run :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
run query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

runSafe :: SqlPersistT IO a -> App (Either String a)
runSafe query =
    (do pool <- asks getPool
        liftIO (Right <$> runSqlPool query pool)) `catch`
    defaultFn
  where
    defaultFn (SomeException e) = return (Left $ displayException e)
