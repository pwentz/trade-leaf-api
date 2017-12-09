{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Trade where

import           Api.Error                   (ApiErr (..), StatusCode (..),
                                              apiErr, sqlError)
import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.Int                    (Int64)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Trade
import           Queries.Trade               (findAccepted, findExchange,
                                              findFromOffers)
import           Servant
import           Utils                       (sHead)

data TradeRequest = TradeRequest
    { acceptedOfferId :: Int64
    , exchangeOfferId :: Int64
    } deriving (Generic)

data TradePatchReq = TradePatchReq
    { isMutual        :: Maybe Bool
    , acceptedOfferId :: Maybe Int64
    , exchangeOfferId :: Maybe Int64
    } deriving (Eq, Show, Generic)

instance FromJSON TradeRequest

instance FromJSON TradePatchReq

type TradeAPI
     = "trades" :> QueryParam "acceptedOfferId" Int64 :> QueryParam "exchangeOfferId" Int64 :> Get '[JSON] (Maybe (Pg.Entity Trade))
     :<|> "trades" :> ReqBody '[JSON] TradeRequest :> Post '[JSON] Int64
     :<|> "trades" :> Capture "id" Int64 :> ReqBody '[JSON] TradePatchReq :> Patch '[JSON] ()

tradeServer :: ServerT TradeAPI App
tradeServer = getTrade :<|> createTrade :<|> patchTrade

getTrade :: Maybe Int64 -> Maybe Int64 -> App (Maybe (Pg.Entity Trade))
getTrade mbAcceptedOfferId mbExchangeOfferId =
    maybe getByExchangeOfferId getByAcceptedOfferId mbAcceptedOfferId
  where
    getByExchangeOfferId =
        maybe
            (return Nothing)
            ((sHead <$>) . findExchange . Pg.toSqlKey)
            mbExchangeOfferId
    getByAcceptedOfferId acceptedOfferId =
        maybe
            (sHead <$> (findAccepted $ Pg.toSqlKey acceptedOfferId))
            (findFromOffers (Pg.toSqlKey acceptedOfferId) . Pg.toSqlKey)
            mbExchangeOfferId

createTrade :: TradeRequest -> App Int64
createTrade TradeRequest {..} = do
    time <- liftIO getCurrentTime
    eitherTrade <-
        Db.runSafe $
        Pg.insert
            Trade
             { tradeAcceptedOfferId = Pg.toSqlKey acceptedOfferId
             , tradeExchangeOfferId = Pg.toSqlKey exchangeOfferId
             , tradeIsMutual = True
             , tradeCreatedAt = time
             , tradeUpdatedAt = time
             }
    either
        (throwError . apiErr . (,) E400 . sqlError)
        (return . Pg.fromSqlKey)
        eitherTrade

patchTrade :: Int64 -> TradePatchReq -> App ()
patchTrade tradeId TradePatchReq {..} = do
    traverse updateIsMutual isMutual
    traverse updateAcceptedOfferId acceptedOfferId
    traverse updateExchangeOfferId exchangeOfferId
    return ()
  where
    updateIsMutual newIsMutual =
        Db.run $
        Pg.update (Pg.toSqlKey tradeId) [TradeIsMutual Pg.=. newIsMutual]
    updateAcceptedOfferId newAcceptedOfferId =
        Db.run $
        Pg.update
            (Pg.toSqlKey tradeId)
            [TradeAcceptedOfferId Pg.=. Pg.toSqlKey newAcceptedOfferId]
    updateExchangeOfferId newExchangeOfferId =
        Db.run $
        Pg.update
            (Pg.toSqlKey tradeId)
            [TradeExchangeOfferId Pg.=. Pg.toSqlKey newExchangeOfferId]
