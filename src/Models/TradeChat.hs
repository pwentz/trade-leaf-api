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
import           Models.Trade

TH.share
  [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
  [TH.persistLowerCase|
  TradeChat json
    tradeId TradeId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
|]

instance Eq TradeChat where
  tc1 == tc2 =
     tradeChatTradeId tc1 == tradeChatTradeId tc2
