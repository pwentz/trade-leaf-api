{-# LANGUAGE RecordWildCards #-}
module Queries.MatchSpec where

import           Config                      (App)
import           Control.Applicative         (liftA2)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coords                 (Coords (Coords))
import           Data.Time                   (getCurrentTime)
import           Database.Persist            (get)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.User
import           Queries.Match
import           SpecHelper                  (runAppToIO, setupTeardown, DbSetup(..), dbSetup)
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

spec :: Spec
spec =
    around setupTeardown $
        describe "Queries.Match" $ do
            it "can get all offers with requests matching category on user offers" $ \config -> do
              DbSetup{..} <- runAppToIO config dbSetup
              matchDescriptions <- runAppToIO config $ do
                  currUser <- Db.run (Sql.get currentUserKey)
                  mbOffers <- traverse findMatches (Sql.Entity currentUserKey <$> currUser)
                  return ((offerDescription . Sql.entityVal <$>) <$> mbOffers)
              matchDescriptions `shouldBe` Just ["water color 30x40 painting", "finger painting dog with lots of colors", "man in rain - watercolor"]
              length <$> matchDescriptions `shouldBe` Just 3
            it "can find a user's matching offers by offer" $ \config -> do
              DbSetup{..} <- runAppToIO config dbSetup
              matchDescriptions <- runAppToIO config $ do
                  currUser <- Db.run (Sql.get currentUserKey)
                  mbOffer <- Db.run $ (Sql.get user4OfferKey)
                  mbMatches <- sequence (liftA2 findUserMatches (Sql.Entity currentUserKey <$> currUser) (Sql.Entity user4OfferKey <$> mbOffer))
                  return $ (fmap . fmap) (offerDescription . Sql.entityVal) mbMatches
              matchDescriptions `shouldBe` Just ["wooden battleship", "wooden rowboat", "wooden canoe"]
