{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Config
import           Control.Exception    (Exception, SomeException (..),
                                       displayException)
import           Control.Monad.Catch  (catch)
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Time
import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           GHC.Generics         (Generic)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
    User json
        username String
        UniqueUsername username
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        password String
        deriving Show
|]

-- doMigrations :: SqlPersistT IO ()
-- doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

runSafeDb :: SqlPersistT IO a -> App (Either String a)
runSafeDb query =
    (do pool <- asks getPool
        liftIO $ (fmap Right $ runSqlPool query pool)) `catch`
    defaultFn
  where
    defaultFn (SomeException e) = return (Left $ displayException e)
