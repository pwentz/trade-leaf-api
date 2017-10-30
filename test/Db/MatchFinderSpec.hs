module Db.MatchFinderSpec where

import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Time                   (getCurrentTime)
import qualified Database.Persist.Postgresql as Sql
import Database.Persist (get)
import           Models                      (Category (Category),
                                              Offer (Offer), Request (Request),
                                              User (User), runDb, offerDescription, userUsername)
import           SpecHelper                  (runAppToIO, setupTeardown)
import           Test.Hspec
import           Test.QuickCheck
import MatchFinder

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
dbSetup :: App User
dbSetup =
  let
    currentUser time =
      User "pat" "password" Nothing "41.938132,-87.642753" time time
  in do
    time <- liftIO getCurrentTime
    handyCatKey <- runDb $ Sql.insert (Category "handywork" time time)
    artCatKey <- runDb $ Sql.insert (Category "decorative art" time time)
    user1Key <- runDb $ Sql.insert (currentUser time)
    offer1Key <- runDb $ Sql.insert (Offer user1Key artCatKey Nothing "painting" 999 time time)
    request1Key <- runDb $ Sql.insert (Request offer1Key handyCatKey "sand fence plz" time time)
    user2Key <- runDb $ Sql.insert (User "fred" "password" Nothing "41.858210,-87.651700" time time)
    offer2Key <- runDb $ Sql.insert (Offer user2Key handyCatKey Nothing "i sand fence" 10 time time)
    request2Key <- runDb $ Sql.insert (Request offer2Key artCatKey "i like painting plz" time time)
    user3Key <- runDb $ Sql.insert (User "bill" "password" Nothing "42.858731,-89.828149" time time)
    offer3Key <- runDb $ Sql.insert (Offer user3Key handyCatKey Nothing "i've sanded once" 1 time time)
    request3Key <- runDb $ Sql.insert (Request offer3Key artCatKey "i've always wanted a painting" time time)
    return (currentUser time)

complexDbSetup :: App User
complexDbSetup =
  let
    currentUser time =
      User "pat" "password" Nothing "41.938132,-87.642753" time time
  in do
    time <- liftIO getCurrentTime
    handyCatKey <- runDb $ Sql.insert (Category "handywork" time time)
    artCatKey <- runDb $ Sql.insert (Category "decorative art" time time)
    babysitCatKey <- runDb $ Sql.insert (Category "babysitter" time time)
    currUserKey <- runDb $ Sql.insert (currentUser time)
    currUserOffer1 <- runDb $ Sql.insert (Offer currUserKey artCatKey Nothing "i offer painting" 999 time time)
    currUserOffer2 <- runDb $ Sql.insert (Offer currUserKey babysitCatKey Nothing "i will sit baby" 15 time time)
    _ <- runDb $ Sql.insert (Request currUserOffer1 handyCatKey "sand fence plz" time time)
    _ <- runDb $ Sql.insert (Request currUserOffer2 artCatKey "i need painting" time time)
    {-| 6 miles from currentUser -}
    user1Key <- runDb $ Sql.insert (User "Otto" "password" Nothing "41.858210,-87.651700" time time)
    user1Offer <- runDb $ Sql.insert (Offer user1Key handyCatKey Nothing "i sand fence" 10 time time)
    _ <- runDb $ Sql.insert (Request user1Offer artCatKey "i like painting plz" time time)
    {-| 4 miles from currentUser -}
    user2Key <- runDb $ Sql.insert (User "Tim" "password" Nothing "41.888730,-87.687969" time time)
    user2Offer <- runDb $ Sql.insert (Offer user2Key handyCatKey Nothing "i've sanded once" 1 time time)
    _ <- runDb $ Sql.insert (Request user2Offer artCatKey "i've always wanted a painting" time time)
    {-| 18 miles from currentUser -}
    user3Key <- runDb $ Sql.insert (User "Theron" "password" Nothing "41.680753,-87.698157" time time)
    user3Offer <- runDb $ Sql.insert (Offer user3Key artCatKey Nothing "warhol" 20 time time)
    _ <- runDb $ Sql.insert (Request user3Offer babysitCatKey "my baby needs sitting" time time)
    {-| 14 miles from currentUser -}
    user4Key <- runDb $ Sql.insert (User "Felicia" "password" Nothing "41.734517,-87.674043" time time)
    user4Offer <- runDb $ Sql.insert (Offer user4Key artCatKey Nothing "i fingerpainted" 10 time time)
    _ <- runDb $ Sql.insert (Request user4Offer babysitCatKey "SIT ON MY BABY" time time)
    {-| 9 miles from currentUser -}
    user5Key <- runDb $ Sql.insert (User "Phil" "password" Nothing "41.804575,-87.671359" time time)
    user5Offer <- runDb $ Sql.insert (Offer user5Key artCatKey Nothing "it is abstract" 10 time time)
    _ <- runDb $ Sql.insert (Request user5Offer babysitCatKey "watch all the kids" time time)
    return (currentUser time)

spec :: Spec
spec = do
    around setupTeardown $ do
        describe "MatchFinderSpec" $ do
            it "can get all offers with requests matching category on user offers" $ \config -> do
              offerDescrips <- runAppToIO config $ do
                  currUser <- dbSetup
                  offers <- findMatches currUser
                  return ((offerDescription . Sql.entityVal) <$> offers)
              offerDescrips `shouldBe` ["i sand fence", "i've sanded once"]
            it "can get all users for offers within a given radius" $ \config -> do
              {-| Can find offers that are within distance of that user's offer radius,
                  but fails to rule out offers that are out of range for currentUser's offer radius (see user3)
              -}
              potentialMatches <- runAppToIO config $ do
                  currUser <- complexDbSetup
                  matchesByUser <- findMatchesInRadius currUser
                  matches <- findWithinRadius currUser matchesByUser
                  return ((offerDescription . Sql.entityVal) <$> matches)
              potentialMatches `shouldBe` ["i sand fence", "it is abstract"]