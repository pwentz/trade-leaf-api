{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Trade where

import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH
import           Models.Offer

TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
    [TH.persistLowerCase|
    Trade json
        offer1Id OfferId
        offer2Id OfferId
        isOpen Bool
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
|]

instance Eq Trade where
    trade1 == trade2 =
        all
            id
            [ tradeOffer1Id trade1 == tradeOffer1Id trade2
            , tradeOffer2Id trade1 == tradeOffer2Id trade2
            , tradeIsOpen trade1 == tradeIsOpen trade2
            ]
