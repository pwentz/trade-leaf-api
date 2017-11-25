{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.User where

import           Data.Coords
import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH
import           Models.Photo

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
        deriving Show
|]

instance Eq User where
    user1 == user2 =
        all
            id
            [ userFirstName user1 == userFirstName user2
            , userLastName user1 == userLastName user2
            , userEmail user1 == userEmail user2
            , userUsername user1 == userUsername user2
            , userPassword user1 == userPassword user2
            , userPhotoId user1 == userPhotoId user2
            , userCoordinates user1 == userCoordinates user2
            ]
