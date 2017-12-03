{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Trade where

import           Api.Error                   (ApiErr (..), StatusCode (..),
                                              apiErr, sqlError)
import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON)
import           Data.Int                    (Int64)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Trade
import           Queries.Trade               (findFromOffers)
import           Servant

data TradeRequest = TradeRequest
    { acceptedOfferId :: Int64
    , exchangeOfferId :: Int64
    } deriving (Eq, Show, Generic)

instance FromJSON TradeRequest

type TradeAPI
    = "trades" :> ReqBody '[JSON] TradeRequest :> Post '[JSON] (Pg.Entity Trade)

tradeServer :: ServerT TradeAPI App
tradeServer = closeOrCreate

closeOrCreate :: TradeRequest -> App (Pg.Entity Trade)
closeOrCreate tradeReq@TradeRequest {..} = do
    existingTrade <- findFromOffers (Pg.toSqlKey acceptedOfferId) (Pg.toSqlKey exchangeOfferId)
    case existingTrade of
        Nothing -> do
            tradeId <- createTrade tradeReq
            mbTrade <- Db.run $ (Pg.get . Pg.toSqlKey) tradeId
            maybe
                (throwError $ apiErr (E500, CustomError "Something went wrong!"))
                (return . Pg.Entity (Pg.toSqlKey tradeId))
                mbTrade
        Just trade@Pg.Entity {..} ->
            makeMutual (Pg.fromSqlKey entityKey) >> return trade

createTrade :: TradeRequest -> App Int64
createTrade TradeRequest {..} = do
    time <- liftIO getCurrentTime
    eitherTrade <-
        Db.runSafe $
        Pg.insert
            (Trade
             { tradeAcceptedOfferId = Pg.toSqlKey acceptedOfferId
             , tradeExchangeOfferId = Pg.toSqlKey exchangeOfferId
             , tradeIsMutual = True
             , tradeCreatedAt = time
             , tradeUpdatedAt = time
             })
    either
        (throwError . apiErr . (,) E400 . sqlError)
        (return . Pg.fromSqlKey)
        eitherTrade

makeMutual :: Int64 -> App ()
makeMutual tradeId = do
    mbTrade <- Db.run $ Pg.get (Pg.toSqlKey tradeId :: Pg.Key Trade)
    case mbTrade of
        Nothing -> throwError $ apiErr (E404, CustomError "Trade not found")
        Just trade -> do
            Db.run $ Pg.update (Pg.toSqlKey tradeId) [TradeIsMutual Pg.=. False]
