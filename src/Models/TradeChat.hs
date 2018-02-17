{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.TradeChat where

import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH
import           Models.Offer

TH.share
  [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
  [TH.persistLowerCase|
  TradeChat json
    offer1Id OfferId
    offer2Id OfferId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
|]

instance Eq TradeChat where
  tc1 == tc2 =
    and
      [ tradeChatOffer1Id tc1 == tradeChatOffer1Id tc2
      , tradeChatOffer2Id tc1 == tradeChatOffer2Id tc2
      ]
