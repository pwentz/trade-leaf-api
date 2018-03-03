{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Message where

import           Data.Time
import           Database.Persist.Sql
import qualified Database.Persist.TH  as TH
import           Models.TradeChat
import           Models.User

TH.share
  [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"]
  [TH.persistLowerCase|
  Message json
    tradeChatId TradeChatId
    senderId UserId
    content String
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
|]

instance Eq Message where
  msg1 == msg2 =
    and
      [ messageTradeChatId msg1 == messageTradeChatId msg2
      , messageSenderId msg1 == messageSenderId msg2
      , messageContent msg1 == messageContent msg2
      ]
