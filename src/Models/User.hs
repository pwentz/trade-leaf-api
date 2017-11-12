{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.User where

import Database.Persist.Sql
import qualified Database.Persist.TH as TH
import Data.Time
import Data.Coords
import Models.Photo


TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
    [TH.persistLowerCase|
    User json
        firstName String
        lastName String
        email String
        UniqueEmail email
        username String
        UniqueUsername username
        password String
        photoId PhotoId Maybe
        coordinates Coords Maybe
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show Eq
|]
