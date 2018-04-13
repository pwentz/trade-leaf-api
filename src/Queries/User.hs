module Queries.User where

import           Config                      (App)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Models.User
import Data.Int (Int64)

findByUsername :: String -> App (Maybe (Sql.Entity User))
findByUsername username =
    Db.run $ Sql.selectFirst [UserUsername Sql.==. username] []

get :: Int64 -> App (Maybe User)
get = Db.run . Sql.get . Sql.toSqlKey
