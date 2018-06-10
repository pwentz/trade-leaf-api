{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Category where

import           Config                      (App)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Models.Category
import           Servant


type CategoryAPI
    = "categories" :> Get '[JSON] [Sql.Entity Category]


categoryServer :: ServerT CategoryAPI App
categoryServer = getCategories


getCategories :: App [Sql.Entity Category]
getCategories =
  Db.run $ select (from return)
