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
        acceptedOfferId OfferId
        exchangeOfferId OfferId
        isMutual Bool
        createdAt UTCTime default=CURRENT_TIMESTAMP
        updatedAt UTCTime default=CURRENT_TIMESTAMP
|]

instance Eq Trade where
    trade1 == trade2 =
        all
            id
            [ tradeAcceptedOfferId trade1 == tradeAcceptedOfferId trade2
            , tradeExchangeOfferId trade1 == tradeExchangeOfferId trade2
            , tradeIsMutual trade1 == tradeIsMutual trade2
            ]
