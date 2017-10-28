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
        password String
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show
    Category json
        name String
        UniqueName name
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show
    Offer json
        userId UserId
        categoryId CategoryId
        cloudinaryId String Maybe
        description String
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show
|]

instance Eq User where
    (User un pw _ _) == (User un' pw' _ _) = (un == un') && (pw == pw')

instance Eq Category where
    (Category name _ _) == (Category name' _ _) = name == name'

instance Eq Offer where
    (Offer uid cid cloudId desc _ _) == (Offer uid' cid' cloudId' desc' _ _) =
        (uid == uid') &&
        (cid == cid') && (cloudId == cloudId') && (desc == desc')

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

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
