{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Photo where

import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH

TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
    [TH.persistLowerCase|
    Photo json
        cloudinaryId String Maybe
        imageUrl String
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show
|]

instance Eq Photo where
    photo1 == photo2 =
        and
            [ photoCloudinaryId photo1 == photoCloudinaryId photo2
            , photoImageUrl photo1 == photoImageUrl photo2
            ]
