{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Request where

import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH
import           Models.Category
import           Models.Offer

TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
    [TH.persistLowerCase|
    Request json
        offerId OfferId
        UniqueOfferId offerId
        categoryId CategoryId
        description String
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
        deriving Show
|]

instance Eq Request where
    req1 == req2 =
        and
            [ requestOfferId req1 == requestOfferId req2
            , requestCategoryId req1 == requestCategoryId req2
            , requestDescription req1 == requestDescription req2
            ]
