module Api.PhotoSpec where

import Test.Hspec
import Test.QuickCheck
import SpecHelper (runAppToIO, setupTeardown)
import Api.Photo (PhotoRequest (..), createPhoto)
import Models (runDb)
import Database.Persist.Sql (fromSqlKey, get, toSqlKey)
import Models (photoImageUrl)


spec :: Spec
spec =
  around setupTeardown $ do
    describe "Photo" $ do
      it "createPhoto" $ \config -> do
        photoUrl <-
          runAppToIO config $ do
            photoId <- createPhoto (PhotoRequest Nothing "https://google.com/image/clown.png")
            mbPhoto <- runDb $ get (toSqlKey photoId)
            return (photoImageUrl <$> mbPhoto)
        photoUrl `shouldBe` (Just "https://google.com/image/clown.png")
