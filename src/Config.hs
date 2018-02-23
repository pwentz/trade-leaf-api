{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config where

import           Control.Exception                    (throwIO)
import           Control.Monad.Catch                  (MonadCatch, MonadThrow)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Safe                                 (readMay)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

newtype App a = App
  { runApp :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Config
             , MonadError ServantErr
             , MonadIO
             , MonadCatch
             , MonadThrow
             )

data Config = Config
  { getPool      :: ConnectionPool
  , getEnv       :: Environment
  , getJwtSecret :: String
  , getWsSecret  :: String
  }

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test =
  connStr "_test" >>= \conn -> runNoLoggingT (createPostgresqlPool conn (envPool Test))
makePool Development =
  connStr "" >>= \conn -> runStdoutLoggingT (createPostgresqlPool conn (envPool Development))
makePool Production = do
  pool <-
    runMaybeT $ do
      let keys = ["host=", "port=", "user=", "password=", "dbname="]
          envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE"]
      envVars <- traverse (MaybeT . lookupEnv) envs
      let prodStr = mconcat . zipWith (<>) keys $ BS.pack <$> envVars
      runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
  case pool of
    Nothing -> throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a

{-| number of pools to use for a given environment. -}
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

connStr :: BS.ByteString -> IO ConnectionString
connStr sfx =
  let buildConnStr user =
        "host=localhost dbname=trade_leaf" <> sfx <> " user=" <> BS.pack user <> " port=5432"
  in buildConnStr <$> lookupRequired "USER"

getConfig :: IO Config
getConfig = do
  env <- lookupSetting "ENV" Development
  pool <- makePool env
  jwtSecret <- lookupRequired "TRADE_LEAF_SECRET"
  wsSecret <- lookupRequired "WS_SECRET"
  return Config {getPool = pool, getEnv = env, getJwtSecret = jwtSecret, getWsSecret = wsSecret}

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing  -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]

{-| Lookup env var and throws if not present. Should only be used for env vars
    that must be present
-}
lookupRequired :: String -> IO String
lookupRequired envVar = do
  maybeSecret <- lookupEnv envVar
  case maybeSecret of
    Nothing -> error $ mconcat ["Missing ", envVar, " environment variable."]
    Just var -> return var
