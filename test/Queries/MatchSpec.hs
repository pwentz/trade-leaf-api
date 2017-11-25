module Queries.MatchSpec where

import           Config                      (App)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Coords                 (Coords (Coords))
import           Data.Time                   (getCurrentTime)
import           Database.Persist            (get)
import qualified Database.Persist.Postgresql as Sql
import qualified Db.Main                     as Db
import           Queries.Match
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Request
import           Models.User
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
dbSetup :: App User
dbSetup =
  let
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time time
  in do
    time <- liftIO getCurrentTime
    photoId <- Db.run $ Sql.insert (Photo Nothing "" time time)
    handyCatKey <- Db.run $ Sql.insert (Category "handywork" time time)
    artCatKey <- Db.run $ Sql.insert (Category "decorative art" time time)
    user1Key <- Db.run $ Sql.insert (currentUser time)
    offer1Key <- Db.run $ Sql.insert (Offer user1Key artCatKey photoId "painting" 999 time time)
    request1Key <- Db.run $ Sql.insert (Request offer1Key handyCatKey "sand fence plz" time time)
    user2Key <- Db.run $ Sql.insert (User "fred" "jones" "fred@gmail.com" "freddy" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time time)
    offer2Key <- Db.run $ Sql.insert (Offer user2Key handyCatKey photoId "i sand fence" 10 time time)
    request2Key <- Db.run $ Sql.insert (Request offer2Key artCatKey "i like painting plz" time time)
    user3Key <- Db.run $ Sql.insert (User "bill" "johnson" "bjohn@gmail.com" "billy" "password" Nothing (Just $ Coords 42.858731 (-89.828149)) time time)
    offer3Key <- Db.run $ Sql.insert (Offer user3Key handyCatKey photoId "i've sanded once" 1 time time)
    request3Key <- Db.run $ Sql.insert (Request offer3Key artCatKey "i've always wanted a painting" time time)
    return (currentUser time)

complexDbSetup :: App User
complexDbSetup =
  let
    currentUser time =
      User "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time time
  in do
    time <- liftIO getCurrentTime
    photoId <- Db.run $ Sql.insert (Photo Nothing "" time time)
    handyCatKey <- Db.run $ Sql.insert (Category "handywork" time time)
    artCatKey <- Db.run $ Sql.insert (Category "decorative art" time time)
    babysitCatKey <- Db.run $ Sql.insert (Category "babysitter" time time)
    currUserKey <- Db.run $ Sql.insert (currentUser time)
    currUserOffer1 <- Db.run $ Sql.insert (Offer currUserKey artCatKey photoId "i offer painting" 999 time time)
    currUserOffer2 <- Db.run $ Sql.insert (Offer currUserKey babysitCatKey photoId "i will sit baby" 15 time time)
    _ <- Db.run $ Sql.insert (Request currUserOffer1 handyCatKey "sand fence plz" time time)
    _ <- Db.run $ Sql.insert (Request currUserOffer2 artCatKey "i need painting" time time)
    {-| 6 miles from currentUser -}
    user1Key <- Db.run $ Sql.insert (User "Otto" "porter" "otto@gmail.com" "ottop" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time time)
    user1Offer <- Db.run $ Sql.insert (Offer user1Key handyCatKey photoId "i sand fence" 10 time time)
    _ <- Db.run $ Sql.insert (Request user1Offer artCatKey "i like painting plz" time time)
    {-| 4 miles from currentUser -}
    user2Key <- Db.run $ Sql.insert (User "Tim" "Thomas" "tim@gmail.com" "timmy_two_time" "password" Nothing (Just $ Coords 41.888730 (-87.687969)) time time)
    user2Offer <- Db.run $ Sql.insert (Offer user2Key handyCatKey photoId "i've sanded once" 1 time time)
    _ <- Db.run $ Sql.insert (Request user2Offer artCatKey "i've always wanted a painting" time time)
    {-| 18 miles from currentUser -}
    user3Key <- Db.run $ Sql.insert (User "Theron" "Mitchell" "theron@gmail.com" "the_ron" "password" Nothing (Just $ Coords 41.680753 (-87.698157)) time time)
    user3Offer <- Db.run $ Sql.insert (Offer user3Key artCatKey photoId "warhol" 20 time time)
    _ <- Db.run $ Sql.insert (Request user3Offer babysitCatKey "my baby needs sitting" time time)
    {-| 14 miles from currentUser -}
    user4Key <- Db.run $ Sql.insert (User "Felicia" "James" "felic@yahoo.com" "fjames" "password" Nothing (Just $ Coords 41.734517 (-87.674043)) time time)
    user4Offer <- Db.run $ Sql.insert (Offer user4Key artCatKey photoId "i fingerpainted" 10 time time)
    _ <- Db.run $ Sql.insert (Request user4Offer babysitCatKey "SIT ON MY BABY" time time)
    {-| 9 miles from currentUser -}
    user5Key <- Db.run $ Sql.insert (User "Phil" "Q" "phil@q.com" "philQ" "password" Nothing (Just $ Coords 41.804575 (-87.671359)) time time)
    user5Offer <- Db.run $ Sql.insert (Offer user5Key artCatKey photoId "it is abstract" 10 time time)
    _ <- Db.run $ Sql.insert (Request user5Offer babysitCatKey "watch all the kids" time time)
    return (currentUser time)

spec :: Spec
spec = do
    around setupTeardown $ do
        describe "Queries.Match" $ do
            it "can get all offers with requests matching category on user offers" $ \config -> do
              matchDescriptions <- runAppToIO config $ do
                  currUser <- dbSetup
                  offers <- findMatches currUser
                  return ((offerDescription . Sql.entityVal) <$> offers)
              matchDescriptions `shouldBe` ["i sand fence", "i've sanded once"]
            it "can find a user's matching offers by offer" $ \config -> do
              matchDescriptions <- runAppToIO config $ do
                  currUser <- complexDbSetup
                  mbOffer <- Db.run $ Sql.selectFirst [OfferDescription Sql.==. "warhol"] []
                  mbMatches <- traverse (findUserMatches currUser . Sql.entityVal) mbOffer
                  return $ (fmap . fmap) (offerDescription . Sql.entityVal) mbMatches
              matchDescriptions `shouldBe` (Just ["i will sit baby"])
