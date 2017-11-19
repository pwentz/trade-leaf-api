{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Photo where

import           Api.Error              (StatusCode (..), apiErr, sqlError)
import           Config                 (App)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON)
import           Data.Int               (Int64)
import           Data.Time              (getCurrentTime)
import qualified Database.Persist.Sql   as Sql
import           GHC.Generics           (Generic)
import qualified Db.Main                as Db
import           Models.Photo           (Photo (Photo))
import           Servant

data PhotoRequest = PhotoRequest
    { cloudinaryId :: Maybe String
    , imageUrl     :: String
    } deriving (Eq, Show, Generic)

instance FromJSON PhotoRequest

type PhotoAPI
    = "photos" :> ReqBody '[ JSON] PhotoRequest :> Post '[ JSON] Int64

photoServer :: ServerT PhotoAPI App
photoServer = createPhoto

createPhoto :: PhotoRequest -> App Int64
createPhoto (PhotoRequest cloudId url) = do
    time <- liftIO getCurrentTime
    eitherPhoto <- Db.runSafe $ Sql.insert (Photo cloudId url time time)
    either
        (throwError . apiErr . ((,) E401) . sqlError)
        (return . Sql.fromSqlKey)
        eitherPhoto
