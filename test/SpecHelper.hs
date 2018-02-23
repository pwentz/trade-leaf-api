module SpecHelper where

import           Config                      (App, Config (..),
                                              Environment (Test), makePool,
                                              runApp)
import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (runReaderT)
import           Data.Coords
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.Trade
import           Models.TradeChat
import           Models.User


runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
    result <- runExceptT $ runReaderT (runApp app) config
    either throwIO return result

setupTeardown :: (Config -> IO a) -> IO ()
setupTeardown runTestsWith = do
    pool <- makePool Test
    migrateDb pool
    cleanDb pool
    runTestsWith Config { getPool = pool
                        , getEnv = Test
                        , getJwtSecret = "trade-leaf-secret" }
    cleanDb pool
  where
    migrateDb :: Pg.ConnectionPool -> IO ()
    migrateDb = Pg.runSqlPool Db.doMigrations
    cleanDb :: Pg.ConnectionPool -> IO ()
    cleanDb pool = do
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Request])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Trade])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter TradeChat])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Offer])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Category])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter User])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Photo])) pool

createUser :: String -> String -> String -> String -> String -> Maybe (Pg.Key Photo) -> Maybe Coords -> UTCTime -> App (Pg.Key User)
createUser fname lname email username pw photoId coords time =
  Db.run $ Pg.insert (newUser fname lname email username pw photoId coords time)

newUser :: String -> String -> String -> String -> String -> Maybe (Pg.Key Photo) -> Maybe Coords -> UTCTime -> User
newUser fname lname email username pw photoId coords time =
    User
      { userFirstName = fname
      , userLastName = lname
      , userEmail = email
      , userUsername = username
      , userPassword = pw
      , userPhotoId = photoId
      , userCoordinates = coords
      , userCreatedAt = time
      , userUpdatedAt = time
      }


createPhoto :: String -> UTCTime -> App (Pg.Key Photo)
createPhoto url time =
  Db.run $ Pg.insert (newPhoto url time)

newPhoto :: String -> UTCTime -> Photo
newPhoto url time =
  Photo
  { photoCloudinaryId = Nothing
  , photoImageUrl = url
  , photoCreatedAt = time
  , photoUpdatedAt = time
  }


createCategory :: String -> UTCTime -> App (Pg.Key Category)
createCategory name time =
  Db.run $ Pg.insert (newCategory name time)

newCategory :: String -> UTCTime -> Category
newCategory name time =
  Category
    { categoryName = name
    , categoryCreatedAt = time
    , categoryUpdatedAt = time
    }


createOffer :: Pg.Key User -> Pg.Key Category -> Pg.Key Photo -> String -> Double -> UTCTime -> App (Pg.Key Offer)
createOffer userKey categoryKey photoKey desc radius time =
  Db.run $ Pg.insert (newOffer userKey categoryKey photoKey desc radius time)

newOffer :: Pg.Key User -> Pg.Key Category -> Pg.Key Photo -> String -> Double -> UTCTime -> Offer
newOffer userKey categoryKey photoKey desc radius time =
  Offer
    { offerUserId = userKey
    , offerCategoryId = categoryKey
    , offerPhotoId = photoKey
    , offerDescription = desc
    , offerRadius = radius
    , offerCreatedAt = time
    , offerUpdatedAt = time
    }

createRequest :: Pg.Key Offer -> Pg.Key Category -> String -> UTCTime -> App (Pg.Key Request)
createRequest offerKey categoryKey desc time =
  Db.run $ Pg.insert (newRequest offerKey categoryKey desc time)

newRequest :: Pg.Key Offer -> Pg.Key Category -> String -> UTCTime -> Request
newRequest offerKey categoryKey desc time =
  Request
    { requestOfferId = offerKey
    , requestCategoryId = categoryKey
    , requestDescription = desc
    , requestCreatedAt = time
    , requestUpdatedAt = time
    }

createTrade :: Pg.Key Offer -> Pg.Key Offer -> Maybe (Pg.Key TradeChat) -> Bool -> UTCTime -> App (Pg.Key Trade)
createTrade acceptedOfferKey exchangeOfferKey tradeChatKey isSuccessful time =
  Db.run $ Pg.insert (newTrade acceptedOfferKey exchangeOfferKey tradeChatKey isSuccessful time)

newTrade :: Pg.Key Offer -> Pg.Key Offer -> Maybe (Pg.Key TradeChat) -> Bool -> UTCTime -> Trade
newTrade acceptedOfferKey exchangeOfferKey tradeChatKey isSuccessful time =
  Trade
    { tradeAcceptedOfferId = acceptedOfferKey
    , tradeExchangeOfferId = exchangeOfferKey
    , tradeTradeChatId = tradeChatKey
    , tradeIsSuccessful = isSuccessful
    , tradeCreatedAt = time
    , tradeUpdatedAt = time
    }

createTradeChat :: Pg.Key Offer -> Pg.Key Offer -> UTCTime -> App (Pg.Key TradeChat)
createTradeChat offer1Key offer2Key time =
  Db.run $ Pg.insert (newTradeChat offer1Key offer2Key time)

newTradeChat :: Pg.Key Offer -> Pg.Key Offer -> UTCTime -> TradeChat
newTradeChat offer1Key offer2Key time =
  TradeChat
    { tradeChatOffer1Id = offer1Key
    , tradeChatOffer2Id = offer2Key
    , tradeChatCreatedAt = time
    , tradeChatUpdatedAt = time
    }
