module Db.MatchFinderSpec where

import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Sql
import           Models                      (Category (Category),
                                              Offer (Offer), Request (Request),
                                              User (User), runDb)
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck

{-|
    As a user,
      If I am offering a painting (category: decorative art),
      and in return I want someone to sand my fence (category: handywork)
      and the radius on my offer is infinite (can mail),
      and there is a user that is offering handywork in return for decorative art,
      and this user has a radius on their offer of 10 miles,
      and this user is 5 miles away,
      I expect to be matched with this user

- For each of my offers (o), find all the offers that have:
    - a category matching the category of the request on o
    - a request that matches the category of o
    - a radius that is <= the radius of o
-}
dbSetup :: App ()
dbSetup = do
    time <- liftIO getCurrentTime
    handyCatKey <- runDb $ Sql.insert (Category "handywork" time time)
    artCatKey <- runDb $ Sql.insert (Category "decorative art" time time)
    user1Key <- runDb $ Sql.insert (User "pat" "password" Nothing "41.938132,-87.642753" time time)
    offer1Key <- runDb $ Sql.insert (Offer user1Key artCatKey Nothing "painting" 999 time time)
    request1Key <- runDb $ Sql.insert (Request offer1Key handyCatKey "sand fence plz" time time)
    user2Key <- runDb $ Sql.insert (User "fred" "password" Nothing "41.858210,-87.651700" time time)
    offer2Key <- runDb $ Sql.insert (Offer user2Key handyCatKey Nothing "i sand fence" 10 time time)
    request2Key <- runDb $ Sql.insert (Request offer2Key artCatKey "i like painting plz" time time)
    return ()

spec :: Spec
spec = do
    around setupTeardown $ do
        describe "MatchFinderSpec" $ do
            it "runs the spec" $ \config -> do
                _ <- runAppToIO config dbSetup
                (4 + 4) `shouldBe` 8
