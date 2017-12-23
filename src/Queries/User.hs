module Queries.User where

import           Config                      (App)
import           Database.Persist.Postgresql
import qualified Db.Main                     as Db
import           Models.User

findByUsername :: String -> App (Maybe (Entity User))
findByUsername username =
    Db.run $ selectFirst [UserUsername ==. username] []
