module Queries.UserSpec where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.User
import           Queries.User
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Queries.User" $ do
      context "findByUsername" $
        it "can get a given user by their username" $ \config -> do
          time <- liftIO getCurrentTime
          requestedUser <-
            Spec.runAppToIO config $ do
              userKey <-
                Spec.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing Nothing time
              findByUsername "pwentz"
          userUsername . Pg.entityVal <$> requestedUser `shouldBe` Just "pwentz"
