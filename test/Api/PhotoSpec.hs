module Api.PhotoSpec where

import           Api.Photo            (PhotoRequest (..), createPhoto, destroyPhoto)
import           Database.Persist.Sql (fromSqlKey, get, toSqlKey, count, Filter)
import qualified Db.Main              as Db
import           Models.Photo
import           SpecHelper           (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around setupTeardown $
    describe "Photo" $ do
      context "createPhoto" $
        it "inserts a new photo into the database" $ \config -> do
          photoUrl <-
            runAppToIO config $ do
              photoId <- createPhoto (PhotoRequest Nothing "https://google.com/image/clown.png")
              mbPhoto <- Db.run $ get (toSqlKey photoId)
              return (photoImageUrl <$> mbPhoto)
          photoUrl `shouldBe` Just "https://google.com/image/clown.png"
      context "destroyPhoto" $
        it "removes the given photo from the database" $ \config -> do
          photoCount <- runAppToIO config $ do
            photoId <- createPhoto (PhotoRequest Nothing "https://google.com/image/clown.png")
            destroyPhoto (toSqlKey photoId)
            Db.run $ count ([] :: [Filter Photo])
          photoCount `shouldBe` 0
