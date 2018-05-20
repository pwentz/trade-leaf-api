module Queries.MessageSpec where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time              (UTCTime (..), fromGregorian,
                                         getCurrentTime, secondsToDiffTime)
import qualified Database.Persist.Sql   as Sql
import qualified Db.Main                as Db
import           Models.Message
import           Queries.Message
import qualified SpecHelper             as Spec
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Queries.Message" $ do
      context "getMessages" $
        it "can retrieve all messages (in order) all messagesfor a given TradeChat" $ \config -> do
          (messages, msg1Key, msg2Key, msg3Key) <-
            Spec.runAppToIO config $ do
              time <- liftIO getCurrentTime
              categoryKey <- Db.createCategory "tutoring" time
              photoKey <- Db.createPhoto "dog.png" time
              user1Key <-
                Db.createUser "fred" "johnson" "fred@gmail.com" "freddy" "password" Nothing Nothing time
              user2Key <-
                Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
              user1Offer <- Db.createOffer user1Key categoryKey photoKey "math" 99 time
              user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
              tradeKey <- Db.createTrade user1Offer user2Offer False time
              tradeChatKey <- Db.createTradeChat tradeKey time
              msg1Key <- Db.createMessage tradeChatKey user1Key "hey bill" (UTCTime (fromGregorian 2018 3 1) (secondsToDiffTime 0))
              msg2Key <- Db.createMessage tradeChatKey user2Key "hi fred" (UTCTime (fromGregorian 2018 3 2) (secondsToDiffTime 350))
              msg3Key <-
                Db.createMessage tradeChatKey user1Key "whatcha doin?" (UTCTime (fromGregorian 2018 3 2) (secondsToDiffTime 100))
              msgs <- getMessages tradeChatKey
              return (msgs, msg1Key, msg2Key, msg3Key)
          Sql.entityKey <$> messages `shouldBe` [msg2Key, msg3Key, msg1Key]
      context "destroyMessage" $
        it "can delete a message record from the data base" $ \config -> do
          msgCount <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Db.createCategory "tutoring" time
            photoKey <- Db.createPhoto "dog.png" time
            user1Key <-
              Db.createUser "fred" "johnson" "fred@gmail.com" "freddy" "password" Nothing Nothing time
            user2Key <-
              Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            user1Offer <- Db.createOffer user1Key categoryKey photoKey "math" 99 time
            user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
            tradeKey <- Db.createTrade user1Offer user2Offer False time
            tradeChatKey <- Db.createTradeChat tradeKey time
            msgKey <- Db.createMessage tradeChatKey user1Key "hey bill" time
            destroyMessage msgKey
            Db.run $ Sql.count ([] :: [Sql.Filter Message])
          msgCount `shouldBe` 0
