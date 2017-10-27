module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              getConfig, lookupSetting,
                                              makePool, setLogger)

main :: IO ()
main = do
    port <- lookupSetting "PORT" 8080
    cfg <- getConfig
    let logger = setLogger $ getEnv cfg
    -- runSqlPool doMigrations $ getPool cfg
    run port $ logger $ app cfg
