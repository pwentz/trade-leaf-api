{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Offer where

import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH
import           Models.Category
import           Models.Photo
import           Models.User

TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
    [TH.persistLowerCase|
    Offer json
        userId UserId
        categoryId CategoryId
        photoId PhotoId
        description String
        radius Double default='Infinity'
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show
|]

instance Eq Offer where
    offer1 == offer2 =
        all
            id
            [ offerUserId offer1 == offerUserId offer2
            , offerCategoryId offer1 == offerCategoryId offer2
            , offerPhotoId offer1 == offerPhotoId offer2
            , offerDescription offer1 == offerDescription offer2
            , offerRadius offer1 == offerRadius offer2
            ]
