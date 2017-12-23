module Queries.UserSpec where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (UTCTime, getCurrentTime)
import qualified Database.Persist.Postgresql as Pg
import qualified Db.Main                     as Db
import           Models.User
import           Queries.User
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

defaultUser :: UTCTime -> User
defaultUser time =
    User
    { userFirstName = "pat"
    , userLastName = "wentz"
    , userEmail = "pat@yahoo.com"
    , userUsername = "pwentz"
    , userPassword = "password"
    , userPhotoId = Nothing
    , userCoordinates = Nothing
    , userCreatedAt = time
    , userUpdatedAt = time
    }

spec :: Spec
spec =
    around setupTeardown $
    describe "Queries.User" $
    it "can get a given user by their username" $ \config -> do
        time <- liftIO getCurrentTime
        requestedUser <- runAppToIO config $ do
            userKey <- Db.run $ Pg.insert (defaultUser time)
            findByUsername "pwentz"
        Pg.entityVal <$> requestedUser `shouldBe` Just (defaultUser time)
