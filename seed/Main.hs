module Main (main) where

import           Config                      (App, Config (..),
                                              Environment (Development),
                                              getConfig, makePool, runApp)
import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (runReaderT)
import           Data.Coords
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
seedDb config =
  runAppToIO config $ do
    time <- liftIO getCurrentTime
    photoKey <- Db.createPhoto "" time
    woodworkingCategoryKey <- Db.createCategory "woodworking" time
    artCategoryKey <- Db.createCategory "decorative art" time
    currentUserKey <- Db.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time
    currentUserOffer1Key <- Db.createOffer currentUserKey artCategoryKey photoKey "some painting" 999 time
    currentUserRequest1Key <- Db.createRequest currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time
    {-| 6 miles from currentUser -}
    user2Key <- Db.createUser "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time
    user2OfferKey <- Db.createOffer user2Key artCategoryKey photoKey "water color 30x40 painting" 10 time
    user2OfferRequestKey <- Db.createRequest user2OfferKey artCategoryKey "animal painting for kid" time
    {-| 14 miles from currentUser -}
    user3Key <- Db.createUser "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" "password" Nothing (Just $ Coords 41.734517 (-87.674043)) time
    user3OfferKey <- Db.createOffer user3Key artCategoryKey photoKey "finger painting dog with lots of colors" 10 time
    user3OfferRequestKey <- Db.createRequest user3OfferKey artCategoryKey "looking for a large painting" time
    {-| 9 miles from currentUser -}
    user4Key <- Db.createUser "Millie" "Bobby Brown" "bobby@brown.com" "milliebob" "password" Nothing (Just $ Coords 41.804575 (-87.671359)) time
    user4OfferKey <- Db.createOffer user4Key artCategoryKey photoKey "man in rain - watercolor" 10 time
    user4OfferRequestKey <- Db.createRequest user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time
    currentUserOffer2Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time
    currentUserRequest2Key <- Db.createRequest currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time
    currentUserOffer3Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time
    currentUserRequest3Key <- Db.createRequest currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time
    currentUserOffer4Key <- Db.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time
    currentUserRequest4Key <- Db.createRequest currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time
    return ()
