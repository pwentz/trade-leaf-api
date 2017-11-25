module Queries.Request where

import           Config                      (App)
import           Database.Persist.Postgresql
import qualified Db.Main                     as Db
import           Models.Offer
import           Models.Request

getOfferRequest :: Key Offer -> App (Maybe (Entity Request))
getOfferRequest offerKey =
    Db.run $ selectFirst [RequestOfferId ==. offerKey] []
