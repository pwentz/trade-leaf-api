{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Db.Main where

import           Config
import           Control.Exception    (Exception, SomeException (..),
                                       displayException)
import           Control.Monad.Catch  (catch)
import           Control.Monad.Reader
import           Data.Coords
import           Data.Time            (UTCTime)
import           Database.Persist.Sql
import qualified Models.Category      as Category
import qualified Models.Message       as Msg
import qualified Models.Offer         as Offer
import qualified Models.Photo         as Photo
import qualified Models.Request       as Request
import qualified Models.Trade         as Trade
import qualified Models.TradeChat     as TradeChat
import qualified Models.User          as User

doMigrations :: SqlPersistT IO ()
doMigrations = do
  runMigration Photo.migrateAll
  runMigration Category.migrateAll
  runMigration User.migrateAll
  runMigration Offer.migrateAll
  runMigration Request.migrateAll
  runMigration Trade.migrateAll
  runMigration TradeChat.migrateAll
  runMigration Msg.migrateAll

run :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
run query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

runSafe :: SqlPersistT IO a -> App (Either String a)
runSafe query =
  (do pool <- asks getPool
      liftIO (Right <$> runSqlPool query pool)) `catch`
  defaultFn
  where
    defaultFn (SomeException e) = return (Left $ displayException e)

createUser :: String -> String -> String -> String -> String -> Maybe (Key Photo.Photo) -> Maybe Coords -> UTCTime -> App (Key User.User)
createUser fname lname email username pw photoId coords time =
  run $ insert (newUser fname lname email username pw photoId coords time)

newUser :: String -> String -> String -> String -> String -> Maybe (Key Photo.Photo) -> Maybe Coords -> UTCTime -> User.User
newUser fname lname email username pw photoId coords time =
    User.User
      { User.userFirstName = fname
      , User.userLastName = lname
      , User.userEmail = email
      , User.userUsername = username
      , User.userPassword = pw
      , User.userPhotoId = photoId
      , User.userCoordinates = coords
      , User.userCreatedAt = time
      , User.userUpdatedAt = time
      }


createPhoto :: String -> UTCTime -> App (Key Photo.Photo)
createPhoto url time =
  run $ insert (newPhoto url time)

newPhoto :: String -> UTCTime -> Photo.Photo
newPhoto url time =
  Photo.Photo
  { Photo.photoCloudinaryId = Nothing
  , Photo.photoImageUrl = url
  , Photo.photoCreatedAt = time
  , Photo.photoUpdatedAt = time
  }


createCategory :: String -> UTCTime -> App (Key Category.Category)
createCategory name time =
  run $ insert (newCategory name time)

newCategory :: String -> UTCTime -> Category.Category
newCategory name time =
  Category.Category
    { Category.categoryName = name
    , Category.categoryCreatedAt = time
    , Category.categoryUpdatedAt = time
    }


createOffer :: Key User.User -> Key Category.Category -> Key Photo.Photo -> String -> Double -> UTCTime -> App (Key Offer.Offer)
createOffer userKey categoryKey photoKey desc radius time =
  run $ insert (newOffer userKey categoryKey photoKey desc radius time)

newOffer :: Key User.User -> Key Category.Category -> Key Photo.Photo -> String -> Double -> UTCTime -> Offer.Offer
newOffer userKey categoryKey photoKey desc radius time =
  Offer.Offer
    { Offer.offerUserId = userKey
    , Offer.offerCategoryId = categoryKey
    , Offer.offerPhotoId = photoKey
    , Offer.offerDescription = desc
    , Offer.offerRadius = radius
    , Offer.offerCreatedAt = time
    , Offer.offerUpdatedAt = time
    }

createRequest :: Key Offer.Offer -> Key Category.Category -> String -> UTCTime -> App (Key Request.Request)
createRequest offerKey categoryKey desc time =
  run $ insert (newRequest offerKey categoryKey desc time)

newRequest :: Key Offer.Offer -> Key Category.Category -> String -> UTCTime -> Request.Request
newRequest offerKey categoryKey desc time =
  Request.Request
    { Request.requestOfferId = offerKey
    , Request.requestCategoryId = categoryKey
    , Request.requestDescription = desc
    , Request.requestCreatedAt = time
    , Request.requestUpdatedAt = time
    }

createTrade :: Key Offer.Offer -> Key Offer.Offer -> Bool -> UTCTime -> App (Key Trade.Trade)
createTrade acceptedOfferKey exchangeOfferKey isSuccessful time =
  run $ insert (newTrade acceptedOfferKey exchangeOfferKey isSuccessful time)

newTrade :: Key Offer.Offer -> Key Offer.Offer -> Bool -> UTCTime -> Trade.Trade
newTrade acceptedOfferKey exchangeOfferKey isSuccessful time =
  Trade.Trade
    { Trade.tradeAcceptedOfferId = acceptedOfferKey
    , Trade.tradeExchangeOfferId = exchangeOfferKey
    , Trade.tradeIsSuccessful = isSuccessful
    , Trade.tradeCreatedAt = time
    , Trade.tradeUpdatedAt = time
    }

createTradeChat :: Key Trade.Trade -> UTCTime -> App (Key TradeChat.TradeChat)
createTradeChat tradeKey time =
  run $ insert (newTradeChat tradeKey time)

newTradeChat :: Key Trade.Trade -> UTCTime -> TradeChat.TradeChat
newTradeChat tradeKey time =
  TradeChat.TradeChat
    { TradeChat.tradeChatTradeId = tradeKey
    , TradeChat.tradeChatCreatedAt = time
    , TradeChat.tradeChatUpdatedAt = time
    }

createMessage :: Key TradeChat.TradeChat -> Key User.User -> String -> UTCTime -> App (Key Msg.Message)
createMessage tradeChatKey senderKey content time =
  run $ insert (newMessage tradeChatKey senderKey content time)

newMessage :: Key TradeChat.TradeChat -> Key User.User -> String -> UTCTime -> Msg.Message
newMessage tradeChatKey senderKey content time =
  Msg.Message
    { Msg.messageTradeChatId = tradeChatKey
    , Msg.messageSenderId = senderKey
    , Msg.messageContent = content
    , Msg.messageCreatedAt = time
    , Msg.messageUpdatedAt = time
    }
