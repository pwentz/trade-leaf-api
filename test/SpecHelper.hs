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
import           Models.Message
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
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Message])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Request])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter TradeChat])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Trade])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Offer])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Category])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter User])) pool
        Pg.runSqlPool (Pg.deleteWhere ([] :: [Pg.Filter Photo])) pool
