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
-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype App a
    = App
    { runApp :: ReaderT Config (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader Config,
                 MonadError ServantErr, MonadIO, MonadCatch, MonadThrow)

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config
    = Config
    { getPool      :: ConnectionPool
    , getEnv       :: Environment
    , getJwtSecret :: String
    }

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> IO ConnectionPool
makePool Test =
    runNoLoggingT (createPostgresqlPool (connStr "_test") (envPool Test))
makePool Development =
    runStdoutLoggingT (createPostgresqlPool (connStr "") (envPool Development))
makePool Production = do
    -- This function makes heavy use of the 'MaybeT' monad transformer, which
    -- might be confusing if you're not familiar with it. It allows us to
    -- combine the effects from 'IO' and the effect of 'Maybe' into a single
    -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
    -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
    -- give us a @Maybe a@, which would make the code quite a bit more
    -- verbose.
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = mconcat . zipWith (<>) keys $ BS.pack <$> envVars
        runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
    case pool of
        -- If we don't have a correct database configuration, we can't
        -- handle that in the program, so we throw an IO exception. This is
        -- one example where using an exception is preferable to 'Maybe' or
        -- 'Either'.
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just a -> return a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=trade_leaf" <> sfx <> " user=patrickwentz port=5432"


getConfig :: IO Config
getConfig = do
  env  <- lookupSetting "ENV" Development
  pool <- makePool env
  jwtSecret <- lookupRequired "TRADE_LEAF_SECRET"
  return $ Config { getPool = pool, getEnv = env, getJwtSecret = jwtSecret }

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

{-| Lookup env var and throws if not present. Should only be used for env vars
    that must be present
-}
lookupRequired :: String -> IO String
lookupRequired envVar = do
  maybeSecret <- lookupEnv envVar
  case maybeSecret of
    Nothing ->
      error $ mconcat ["Missing ", envVar, " environment variable."]
    Just var ->
      return var