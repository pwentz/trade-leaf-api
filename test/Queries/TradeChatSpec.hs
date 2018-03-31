module Queries.TradeChatSpec where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time              (getCurrentTime)
import qualified Database.Persist.Sql   as Sql
import qualified Db.Main                as Db
import           Models.TradeChat
import           Queries.TradeChat
import qualified SpecHelper             as Spec
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Map as Map
import Models.User
import Models.Message
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Config (App)

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Queries.TradeChat" $ do
      context "findByTrade" $
        it "finds a trade chat for a given trade" $ \config -> do
          (foundTradeChat, tradeChatKey) <- Spec.runAppToIO config $ do
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
            foundTradeChat <- findByTrade tradeKey
            return (foundTradeChat, tradeChatKey)
          Sql.entityKey <$> foundTradeChat `shouldBe` Just tradeChatKey
      context "findByUser" $
        it "can find all of the trade chats that given user is involved in" $ \config -> do
          (tradeChats, tradeChat1Key, tradeChat2Key) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Spec.createPhoto "cat.png" time
            currentUserKey <- Spec.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            user2Key <- Spec.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
            user3Key <- Spec.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            categoryKey <- Spec.createCategory "tutor" time
            currentUserOfferKey <- Spec.createOffer currentUserKey categoryKey photoKey "chemistry" 1 time
            user2OfferKey <- Spec.createOffer user2Key categoryKey photoKey "physics" 1 time
            user3OfferKey <- Spec.createOffer user3Key categoryKey photoKey "calculus" 1 time
            trade1Key <- Spec.createTrade currentUserOfferKey user2OfferKey False time
            tradeChat1Key <- Spec.createTradeChat trade1Key time
            trade2Key <- Spec.createTrade user3OfferKey currentUserOfferKey False time
            tradeChat2Key <- Spec.createTradeChat trade2Key time
            trade3Key <- Spec.createTrade user2OfferKey user3OfferKey False time
            tradeChat3Key <- Spec.createTradeChat trade3Key time
            tradeChats <- findByUser currentUserKey
            return (tradeChats, tradeChat1Key, tradeChat2Key)
          tradeChats `shouldBe` [tradeChat1Key, tradeChat2Key]
      context "findRecipient" $
        it "finds the users in a given chat" $ \config -> do
          (recipientKeys, currentUserKey, user2Key) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Spec.createPhoto "cat.png" time
            currentUserKey <- Spec.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            user2Key <- Spec.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
            user3Key <- Spec.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            categoryKey <- Spec.createCategory "tutor" time
            currentUserOfferKey <- Spec.createOffer currentUserKey categoryKey photoKey "chemistry" 1 time
            user2OfferKey <- Spec.createOffer user2Key categoryKey photoKey "physics" 1 time
            user3OfferKey <- Spec.createOffer user3Key categoryKey photoKey "calculus" 1 time
            trade1Key <- Spec.createTrade currentUserOfferKey user2OfferKey False time
            tradeChat1Key <- Spec.createTradeChat trade1Key time
            recipientKeys <- findRecipients tradeChat1Key
            return (recipientKeys, currentUserKey, user2Key)
          recipientKeys `shouldBe` (currentUserKey, user2Key)
      context "findChatData" $
        it "gets chat data for a given user" $ \config ->
          let
            extractMessage :: Sql.Key Message -> App (Sql.Entity Message)
            extractMessage msgKey = do
              mbMsg <- Db.run (Sql.get msgKey)
              return $ fromJust (Sql.Entity msgKey <$> mbMsg)
            expected :: (Sql.Key TradeChat, Sql.Key User, [Sql.Key Message]) -> (Sql.Key TradeChat, Sql.Key User, [Sql.Key Message]) -> App (Map.Map Int64 ChatData)
            expected chatData1 chatData2 =
              foldr
                (\(tradeChatKey, userKey, messageKeys) acc -> do
                  messages <- traverse extractMessage messageKeys
                  Map.insert (Sql.fromSqlKey tradeChatKey) (ChatData (Sql.fromSqlKey userKey) messages) <$> acc
                 )
                 (return Map.empty)
                 [chatData1, chatData2]
          in do
          (chatData, expectedRes) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Spec.createPhoto "cat.png" time
            currentUserKey <- Spec.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            user2Key <- Spec.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
            user3Key <- Spec.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            categoryKey <- Spec.createCategory "tutor" time
            currentUserOfferKey <- Spec.createOffer currentUserKey categoryKey photoKey "chemistry" 1 time
            user2OfferKey <- Spec.createOffer user2Key categoryKey photoKey "physics" 1 time
            user3OfferKey <- Spec.createOffer user3Key categoryKey photoKey "calculus" 1 time
            trade1Key <- Spec.createTrade currentUserOfferKey user2OfferKey False time
            tradeChat1Key <- Spec.createTradeChat trade1Key time
            trade2Key <- Spec.createTrade user3OfferKey currentUserOfferKey False time
            tradeChat2Key <- Spec.createTradeChat trade2Key time
            trade3Key <- Spec.createTrade user2OfferKey user3OfferKey False time
            tradeChat3Key <- Spec.createTradeChat trade3Key time
            message1Key <- Spec.createMessage tradeChat1Key currentUserKey "yo fred" time
            message2Key <- Spec.createMessage tradeChat2Key currentUserKey "yo bill" time
            message3Key <- Spec.createMessage tradeChat3Key user2Key "hey bill, it's me fred." time
            message4Key <- Spec.createMessage tradeChat1Key user2Key "hello, ed" time
            chatData <- findChatData currentUserKey
            expectedRes <- expected (tradeChat1Key, user2Key, [message1Key, message4Key]) (tradeChat2Key, user3Key, [message2Key])
            return (chatData, expectedRes)
          chatData `shouldBe` expectedRes
