{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Request where

import           Config                      (App)
import           Data.Aeson                  (ToJSON)
import           Data.Int                    (Int64)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           GHC.Generics                (Generic)
import           Models.Category
import           Models.Request

data RequestResponse = RequestResponse
    { id          :: Int64
    , offerId     :: Int64
    , category    :: String
    , description :: String
    } deriving (Eq, Show, Generic)

instance ToJSON RequestResponse

toRequestResponse :: Pg.Entity Request -> App (Maybe RequestResponse)
toRequestResponse (Pg.Entity reqKey reqVal) = do
    reqCat <- (categoryName <$>) <$> (Db.run . Pg.get . requestCategoryId) reqVal
    case reqCat of
        Nothing -> return Nothing
        Just catNm ->
            (return . return)
                RequestResponse
                { Api.Request.id = Pg.fromSqlKey reqKey
                , offerId = Pg.fromSqlKey (requestOfferId reqVal)
                , category = catNm
                , description = requestDescription reqVal
                }
