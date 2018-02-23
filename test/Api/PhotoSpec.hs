module Api.PhotoSpec where

import           Api.Photo            (PhotoRequest (..), createPhoto)
import           Database.Persist.Sql (fromSqlKey, get, toSqlKey)
import qualified Db.Main              as Db
import           Models.Photo         (photoImageUrl)
import           SpecHelper           (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around setupTeardown $
  describe "Photo" $
  it "createPhoto" $ \config -> do
    photoUrl <-
      runAppToIO config $ do
        photoId <- createPhoto (PhotoRequest Nothing "https://google.com/image/clown.png")
        mbPhoto <- Db.run $ get (toSqlKey photoId)
        return (photoImageUrl <$> mbPhoto)
    photoUrl `shouldBe` Just "https://google.com/image/clown.png"
