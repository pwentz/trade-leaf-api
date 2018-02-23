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
import qualified SpecHelper                  as Spec
import           Test.Hspec
import           Test.QuickCheck

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
dbSetup = do
    time <- liftIO getCurrentTime
    photoKey <- Spec.createPhoto "" time
    woodworkingCategoryKey <- Spec.createCategory "woodworking" time
    artCategoryKey <- Spec.createCategory "decorative art" time
    currentUserKey <- Spec.createUser "pat" "wentz" "pat@yahoo.com" "pwentz" "password" Nothing (Just $ Coords 41.938132 (-87.642753)) time
    currentUserOffer1Key <- Spec.createOffer currentUserKey artCategoryKey photoKey "some painting" 999 time
    currentUserRequest1Key <- Spec.createRequest currentUserOffer1Key artCategoryKey "looking for nice painting i can hang in office" time
    {-| 6 miles from currentUser -}
    user2Key <- Spec.createUser "Fred" "Johnson" "fjohn@gmail.com" "freddyjohn" "password" Nothing (Just $ Coords 41.858210 (-87.651700)) time
    user2OfferKey <- Spec.createOffer user2Key artCategoryKey photoKey "water color 30x40 painting" 10 time
    user2OfferRequestKey <- Spec.createRequest user2OfferKey artCategoryKey "animal painting for kid" time
    {-| 14 miles from currentUser -}
    user3Key <- Spec.createUser "Crack" "Jackson" "crackjack@gmail.com" "crackjack1" "password" Nothing (Just $ Coords 41.734517 (-87.674043)) time
    user3OfferKey <- Spec.createOffer user3Key artCategoryKey photoKey "finger painting dog with lots of colors" 10 time
    user3OfferRequestKey <- Spec.createRequest user3OfferKey artCategoryKey "looking for a large painting" time
    {-| 9 miles from currentUser -}
    user4Key <- Spec.createUser "Millie" "Bobby Brown" "bobby@brown.com" "milliebob" "password" Nothing (Just $ Coords 41.804575 (-87.671359)) time
    user4OfferKey <- Spec.createOffer user4Key artCategoryKey photoKey "man in rain - watercolor" 10 time
    user4OfferRequestKey <- Spec.createRequest user4OfferKey woodworkingCategoryKey "looking for a wooden ship" time
    currentUserOffer2Key <- Spec.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden battleship" 999 time
    currentUserRequest2Key <- Spec.createRequest currentUserOffer2Key artCategoryKey "watercolor painting of some weather" time
    currentUserOffer3Key <- Spec.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden rowboat" 999 time
    currentUserRequest3Key <- Spec.createRequest currentUserOffer3Key artCategoryKey "painting that will make me feel cozy" time
    currentUserOffer4Key <- Spec.createOffer currentUserKey woodworkingCategoryKey photoKey "wooden canoe" 999 time
    currentUserRequest4Key <- Spec.createRequest currentUserOffer4Key artCategoryKey "watercolor with a dope vibe" time
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
  around Spec.setupTeardown $
  describe "Queries.Match" $ do
    it "can get all offers with requests matching category on user offers" $ \config -> do
      DbSetup {..} <- Spec.runAppToIO config dbSetup
      matchDescriptions <-
        Spec.runAppToIO config $ do
          currUser <- Db.run (Sql.get currentUserKey)
          mbOffers <- traverse findMatches (Sql.Entity currentUserKey <$> currUser)
          return (fmap (offerDescription . Sql.entityVal) <$> mbOffers)
      case matchDescriptions of
        Nothing -> expectationFailure "Expected value to be present, but there was nothing"
        Just xs ->
          xs `shouldMatchList`
          [ "water color 30x40 painting"
          , "finger painting dog with lots of colors"
          , "man in rain - watercolor"
          ]
    it "can find a user's matching offers by offer" $ \config -> do
      DbSetup {..} <- Spec.runAppToIO config dbSetup
      matchDescriptions <-
        Spec.runAppToIO config $ do
          currUser <- Db.run (Sql.get currentUserKey)
          mbOffer <- Db.run (Sql.get user4OfferKey)
          mbMatches <-
            sequence
              (liftA2
                 findUserMatches
                 (Sql.Entity currentUserKey <$> currUser)
                 (Sql.Entity user4OfferKey <$> mbOffer))
          return $ (fmap . fmap) (offerDescription . Sql.entityVal) mbMatches
      matchDescriptions `shouldBe` Just ["wooden battleship", "wooden rowboat", "wooden canoe"]
