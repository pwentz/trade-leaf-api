module Main (main) where

import           Api.Auth                    (encodePassword)
import           Config                      (App, Config (..),
                                              Environment (Development),
                                              getConfig, makePool, runApp)
import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (runReaderT)
import qualified Data.ByteString.Char8       as BS
import           Data.Coords
import           Data.Maybe                  (fromJust)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Message
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.Trade
import           Models.TradeChat
import           Models.User


main :: IO ()
main = do
  pool <- makePool Development
  Pg.runSqlPool Db.doMigrations pool
  cleanDb pool
  seedDb Config { getPool = pool
                , getEnv = Development
                , getJwtSecret = "trade-leaf-secret"
                }
    where
        cleanDb :: Pg.ConnectionPool -> IO ()
        cleanDb pool = do
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Message])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Request])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter TradeChat])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Trade])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Offer])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Category])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter User])) pool
          Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Photo])) pool


runAppToIO :: Config -> App () -> IO ()
runAppToIO config app = do
  result <- runExceptT $ runReaderT (runApp app) config
  either throwIO return result


seedDb :: Config -> IO ()
seedDb config = do
  encodedPassword <- BS.unpack . fromJust <$> encodePassword "password"
  runAppToIO config $ do
    time <- liftIO getCurrentTime
    photoKey <- Db.createPhoto "" time
    fingerPaintingPupPhotoKey <- Db.createPhoto "http://res.cloudinary.com/tradeleaf/image/upload/v1511638923/finger-painting-dog_vm26xw.jpg" time
    waterColorPaintingPhotoKey <- Db.createPhoto "http://res.cloudinary.com/tradeleaf/image/upload/v1512865187/c075786ba959a19c4b49abec03bc5d3c_ggvnvx.jpg" time
    manInRainPhotoKey <- Db.createPhoto "http://res.cloudinary.com/tradeleaf/image/upload/v1511638661/rain-watercolor_kunesi.jpg" time
    woodworkingCategoryKey <- Db.createCategory "woodworking" time
    artCategoryKey <- Db.createCategory "decorative art" time
    currentUserKey <- Db.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" encodedPassword Nothing (Just $ Coords 41.938132 (-87.642753)) time
    currentUserOffer1Key <- Db.createOffer currentUserKey artCategoryKey photoKey "some painting" 999 time
    currentUserRequest1Key <- Db.createRequest currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time
    {-| 6 miles from currentUser -}
    user2Key <- Db.createUser "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" encodedPassword Nothing (Just $ Coords 37.785834 (-122.406417)) time
    user2OfferKey <- Db.createOffer user2Key artCategoryKey waterColorPaintingPhotoKey "water color 30x40 painting" 999 time
    user2OfferRequestKey <- Db.createRequest user2OfferKey artCategoryKey "animal painting for kid" time
    {-| 14 miles from currentUser -}
    user3Key <- Db.createUser "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" encodedPassword Nothing (Just $ Coords 37.785834 (-122.406417)) time
    user3OfferKey <- Db.createOffer user3Key artCategoryKey fingerPaintingPupPhotoKey "finger painting dog with lots of colors" 999 time
    user3OfferRequestKey <- Db.createRequest user3OfferKey artCategoryKey "looking for a large painting" time
    {-| 9 miles from currentUser -}
    user4Key <- Db.createUser "Millie" "Bobby Brown" "bobby@brown.com" "milliebob" encodedPassword Nothing (Just $ Coords 37.785834 (-122.406417)) time
    user4OfferKey <- Db.createOffer user4Key artCategoryKey manInRainPhotoKey "man in rain - watercolor" 999 time
    user4OfferRequestKey <- Db.createRequest user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time
    currentUserOffer2Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time
    currentUserRequest2Key <- Db.createRequest currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time
    currentUserOffer3Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time
    currentUserRequest3Key <- Db.createRequest currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time
    currentUserOffer4Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time
    currentUserRequest4Key <- Db.createRequest currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time
    tradeKey <- Db.createTrade currentUserOffer2Key user4OfferKey False time
    return ()
