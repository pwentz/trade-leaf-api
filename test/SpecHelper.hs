module SpecHelper where

import           Config                      (App, Config (..),
                                              Environment (Test), makePool,
                                              runApp)
import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (runReaderT)
import           Data.Coords
import           Data.Time                   (getCurrentTime)
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

data DbSetup = DbSetup
    { currentUserKey       :: Pg.Key User
    , currentUserOffer1Key :: Pg.Key Offer
    , currentUserOffer2Key :: Pg.Key Offer
    , currentUserOffer3Key :: Pg.Key Offer
    , currentUserOffer4Key :: Pg.Key Offer
    , user2Key             :: Pg.Key User
    , user2OfferKey        :: Pg.Key Offer
    , user3Key             :: Pg.Key User
    , user3OfferKey        :: Pg.Key Offer
    , user4Key             :: Pg.Key User
    , user4OfferKey        :: Pg.Key Offer
    }

dbSetup :: App DbSetup
dbSetup =
  let
    currentUser time =
      User
        { userFirstName = "pat"
        , userLastName = "wentz"
        , userEmail = "pat@yahoo.com"
        , userUsername = "pwentz"
        , userPassword = "password"
        , userPhotoId = Nothing
        , userCoordinates = (Just $ Coords 41.938132 (-87.642753))
        , userCreatedAt = time
        , userUpdatedAt = time
        }
  in do
    time <- liftIO getCurrentTime
    photoKey <- Db.run $ Pg.insert (Photo Nothing "" time time)
    woodworkingCategoryKey <- Db.run $ Pg.insert (Category "woodworking" time time)
    artCategoryKey <- Db.run $ Pg.insert (Category "decorative art" time time)
    currentUserKey <- Db.run $ Pg.insert (currentUser time)
    currentUserOffer1Key <- Db.run $ Pg.insert (Offer currentUserKey artCategoryKey photoKey "some painting" 999 time time)
    currentUserRequest1Key <- Db.run $ Pg.insert (Request currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time time)
    {-| 6 miles from currentUser -}
    user2Key <- Db.run $ Pg.insert (User "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time time)
    user2OfferKey <- Db.run $ Pg.insert (Offer user2Key artCategoryKey photoKey "water color 30x40 painting" 10 time time)
    user2OfferRequestKey <- Db.run $ Pg.insert (Request user2OfferKey artCategoryKey "animal painting for kid" time time)
    {-| 14 miles from currentUser -}
    user3Key <- Db.run $ Pg.insert (User "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" "password" Nothing (Just $ Coords 41.734517 (-87.674043)) time time)
    user3OfferKey <- Db.run $ Pg.insert (Offer user3Key artCategoryKey photoKey "finger painting dog with lots of colors" 10 time time)
    user3OfferRequestKey <- Db.run $ Pg.insert (Request user3OfferKey artCategoryKey "looking for a large painting" time time)
    {-| 9 miles from currentUser -}
    user4Key <- Db.run $ Pg.insert (User "Millie" "Rock" "bobby@brown.com" "milliebob" "password" Nothing (Just $ Coords 41.804575 (-87.671359)) time time)
    user4OfferKey <- Db.run $ Pg.insert (Offer user4Key artCategoryKey photoKey "man in rain - watercolor" 10 time time)
    user4OfferRequestKey <- Db.run $ Pg.insert (Request user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time time)
    currentUserOffer2Key <- Db.run $ Pg.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time time)
    currentUserRequest2Key <- Db.run $ Pg.insert (Request currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time time)
    currentUserOffer3Key <- Db.run $ Pg.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time time)
    currentUserRequest3Key <- Db.run $ Pg.insert (Request currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time time)
    currentUserOffer4Key <- Db.run $ Pg.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time time)
    currentUserRequest4Key <- Db.run $ Pg.insert (Request currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time time)
    return DbSetup
            { currentUserKey = currentUserKey
            , currentUserOffer1Key = currentUserOffer1Key
            , currentUserOffer2Key = currentUserOffer2Key
            , currentUserOffer3Key = currentUserOffer3Key
            , currentUserOffer4Key = currentUserOffer4Key
            , user2Key = user2Key
            , user2OfferKey = user2OfferKey
            , user3Key = user3Key
            , user3OfferKey = user3OfferKey
            , user4Key = user4Key
            , user4OfferKey = user4OfferKey
            }
