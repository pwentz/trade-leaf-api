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
import           Models.TradeChat

TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
    [TH.persistLowerCase|
      Trade json
          acceptedOfferId OfferId
          exchangeOfferId OfferId
          tradeChatId TradeChatId Maybe
          isSuccessful Bool default=FALSE
          createdAt UTCTime default=CURRENT_TIMESTAMP
          updatedAt UTCTime default=CURRENT_TIMESTAMP
          TradeOfferIds acceptedOfferId exchangeOfferId
          deriving Show
|]

instance Eq Trade where
  trade1 == trade2 =
    and
      [ tradeAcceptedOfferId trade1 == tradeAcceptedOfferId trade2
      , tradeExchangeOfferId trade1 == tradeExchangeOfferId trade2
      , tradeTradeChatId trade1 == tradeTradeChatId trade2
      , tradeIsSuccessful trade1 == tradeIsSuccessful trade2
      ]
