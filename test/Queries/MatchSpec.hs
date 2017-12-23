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
data DbSetup = DbSetup
    { currentUserKey       :: Sql.Key User
    , currentUserOffer1Key :: Sql.Key Offer
    , currentUserOffer2Key :: Sql.Key Offer
    , currentUserOffer3Key :: Sql.Key Offer
    , currentUserOffer4Key :: Sql.Key Offer
    , user2Key             :: Sql.Key User
    , user2OfferKey        :: Sql.Key Offer
    , user3Key             :: Sql.Key User
    , user3OfferKey        :: Sql.Key Offer
    , user4Key             :: Sql.Key User
    , user4OfferKey        :: Sql.Key Offer
    }

dbSetup :: App DbSetup
dbSetup =
  let
    coords =
      Coords 41.938132 (-87.642753)
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just coords) time time
  in do
    time <- liftIO getCurrentTime
    photoKey <- Db.run $ Sql.insert (Photo Nothing "" time time)
    woodworkingCategoryKey <- Db.run $ Sql.insert (Category "woodworking" time time)
    artCategoryKey <- Db.run $ Sql.insert (Category "decorative art" time time)
    currentUserKey <- Db.run $ Sql.insert (currentUser time)
    currentUserOffer1Key <- Db.run $ Sql.insert (Offer currentUserKey artCategoryKey photoKey "some painting" 999 time time)
    currentUserRequest1Key <- Db.run $ Sql.insert (Request currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time time)
    user2Key <- Db.run $ Sql.insert (User "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" "password" Nothing (Just coords) time time)
    user2OfferKey <- Db.run $ Sql.insert (Offer user2Key artCategoryKey photoKey "water color 30x40 painting" 999 time time)
    user2OfferRequestKey <- Db.run $ Sql.insert (Request user2OfferKey artCategoryKey "animal painting for kid" time time)
    user3Key <- Db.run $ Sql.insert (User "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" "password" Nothing (Just coords) time time)
    user3OfferKey <- Db.run $ Sql.insert (Offer user3Key artCategoryKey photoKey "finger painting dog with lots of colors" 999 time time)
    user3OfferRequestKey <- Db.run $ Sql.insert (Request user3OfferKey artCategoryKey "looking for a large painting" time time)
    user4Key <- Db.run $ Sql.insert (User "Millie" "Rock" "bobby@brown.com" "milliebob" "password" Nothing (Just coords) time time)
    user4OfferKey <- Db.run $ Sql.insert (Offer user4Key artCategoryKey photoKey "man in rain - watercolor" 999 time time)
    user4OfferRequestKey <- Db.run $ Sql.insert (Request user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time time)
    currentUserOffer2Key <- Db.run $ Sql.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time time)
    currentUserRequest2Key <- Db.run $ Sql.insert (Request currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time time)
    currentUserOffer3Key <- Db.run $ Sql.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time time)
    currentUserRequest3Key <- Db.run $ Sql.insert (Request currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time time)
    currentUserOffer4Key <- Db.run $ Sql.insert (Offer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time time)
    currentUserRequest4Key <- Db.run $ Sql.insert (Request currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time time)
    return DbSetup
            { currentUserKey = currentUserKey
            , currentUserOffer1Key = currentUserOffer1Key
            , currentUserOffer2Key = currentUserOffer2Key
            , currentUserOffer3Key = currentUserOffer3Key
            , currentUserOffer4Key = currentUserOffer4Key
            , user2Key = user2Key
            , user2OfferKey = user2OfferKey
            , user3Key = user3Key
            , user3OfferKey = user3OfferKey
            , user4Key = user4Key
            , user4OfferKey = user4OfferKey
            }

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
