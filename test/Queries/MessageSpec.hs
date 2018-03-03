module Queries.MessageSpec where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time              (UTCTime(..), getCurrentTime, fromGregorian, secondsToDiffTime)
import qualified Database.Persist.Sql   as Sql
import qualified Db.Main                as Db
import           Models.Category
import           Models.Offer
import           Models.Photo
import           Models.Trade
import           Models.TradeChat
import           Models.User
import           Queries.Message
import qualified SpecHelper             as Spec
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Queries.Message" $
      it "can retrieve all messages (in order) all messagesfor a given TradeChat" $ \config -> do
        (messages, msg1Key, msg2Key, msg3Key) <-
          Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Spec.createCategory "tutoring" time
            photoKey <- Spec.createPhoto "dog.png" time
            user1Key <-
              Spec.createUser "fred" "johnson" "fred@gmail.com" "freddy" "password" Nothing Nothing time
            user2Key <-
              Spec.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            user1Offer <- Spec.createOffer user1Key categoryKey photoKey "math" 99 time
            user2Offer <- Spec.createOffer user2Key categoryKey photoKey "physics" 99 time
            tradeKey <- Spec.createTrade user1Offer user2Offer False time
            tradeChatKey <- Spec.createTradeChat tradeKey time
            msg1Key <- Spec.createMessage tradeChatKey user1Key "hey bill" (UTCTime (fromGregorian 2018 3 1) (secondsToDiffTime 0))
            msg2Key <- Spec.createMessage tradeChatKey user2Key "hi fred" (UTCTime (fromGregorian 2018 3 2) (secondsToDiffTime 350))
            msg3Key <-
              Spec.createMessage tradeChatKey user1Key "whatcha doin?" (UTCTime (fromGregorian 2018 3 2) (secondsToDiffTime 100))
            msgs <- getMessages tradeChatKey
            return (msgs, msg1Key, msg2Key, msg3Key)
        Sql.entityKey <$> messages `shouldBe` [msg1Key, msg3Key, msg2Key]
