module Queries.TradeChatSpec where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time              (UTCTime (..), fromGregorian,
                                         getCurrentTime, secondsToDiffTime)
import qualified Database.Persist.Sql   as Sql
import qualified Db.Main                as Db
import Data.Maybe (fromJust)
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
      context "findByTrade" $ do
        it "finds a trade chat for a given trade" $ \config -> do
          (foundTradeChat, tradeChatKey) <- Spec.runAppToIO config $ do
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
            foundTradeChat <- findByTrade tradeKey
            return (foundTradeChat, tradeChatKey)
          Sql.entityKey <$> foundTradeChat `shouldBe` Just tradeChatKey
        it "returns Nothing if given trade does not have a tradeChat" $ \config -> do
          foundTradeChat <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Db.createCategory "tutoring" time
            photoKey <- Db.createPhoto "dog.png" time
            user1Key <-
              Db.createUser "fred" "johnson" "fred@gmail.com" "freddy" "password" Nothing Nothing time
            user2Key <-
              Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            user3Key <-
              Db.createUser "jan" "janson" "jan@yahoo.com" "jan-jan" "password" Nothing Nothing time
            user1Offer <- Db.createOffer user1Key categoryKey photoKey "math" 99 time
            user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
            user3Offer <- Db.createOffer user2Key categoryKey photoKey "geometry" 99 time
            trade1Key <- Db.createTrade user1Offer user2Offer False time
            trade2Key <- Db.createTrade user1Offer user3Offer False time
            tradeChatKey <- Db.createTradeChat trade1Key time
            findByTrade trade2Key
          foundTradeChat `shouldBe` Nothing

      context "findByUser" $
        it "can find all of the trade chats that given user is involved in" $ \config -> do
          (tradeChats, tradeChat1Key, tradeChat2Key) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.createPhoto "cat.png" time
            currentUserKey <- Db.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            user2Key <- Db.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
            user3Key <- Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            categoryKey <- Db.createCategory "tutor" time
            currentUserOfferKey <- Db.createOffer currentUserKey categoryKey photoKey "chemistry" 1 time
            user2OfferKey <- Db.createOffer user2Key categoryKey photoKey "physics" 1 time
            user3OfferKey <- Db.createOffer user3Key categoryKey photoKey "calculus" 1 time
            trade1Key <- Db.createTrade currentUserOfferKey user2OfferKey False time
            tradeChat1Key <- Db.createTradeChat trade1Key time
            trade2Key <- Db.createTrade user3OfferKey currentUserOfferKey False time
            tradeChat2Key <- Db.createTradeChat trade2Key time
            trade3Key <- Db.createTrade user2OfferKey user3OfferKey False time
            tradeChat3Key <- Db.createTradeChat trade3Key time
            tradeChats <- findByUser currentUserKey
            return (tradeChats, tradeChat1Key, tradeChat2Key)
          tradeChats `shouldBe` [tradeChat1Key, tradeChat2Key]
      context "findRecipient" $
        it "finds the users in a given chat" $ \config -> do
          (recipientKeys, currentUserKey, user2Key) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.createPhoto "cat.png" time
            currentUserKey <- Db.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            user2Key <- Db.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
            user3Key <- Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            categoryKey <- Db.createCategory "tutor" time
            currentUserOfferKey <- Db.createOffer currentUserKey categoryKey photoKey "chemistry" 1 time
            user2OfferKey <- Db.createOffer user2Key categoryKey photoKey "physics" 1 time
            user3OfferKey <- Db.createOffer user3Key categoryKey photoKey "calculus" 1 time
            trade1Key <- Db.createTrade currentUserOfferKey user2OfferKey False time
            tradeChat1Key <- Db.createTradeChat trade1Key time
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
                  tradeChat <- fromJust <$> Db.run (Sql.get tradeChatKey) :: App TradeChat
                  messages <- traverse extractMessage messageKeys
                  Map.insert (Sql.fromSqlKey tradeChatKey) (ChatData (Sql.fromSqlKey tradeChatKey) (Sql.fromSqlKey userKey) messages (Sql.fromSqlKey $ tradeChatTradeId tradeChat) (tradeChatCreatedAt tradeChat) (tradeChatUpdatedAt tradeChat)) <$> acc
                 )
                 (return Map.empty)
                 [chatData1, chatData2]
          in do
          (chatData, expectedRes) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            photoKey <- Db.createPhoto "cat.png" time
            currentUserKey <- Db.createUser "ed" "griswold" "eg@yahoo.com" "grissy1" "pass" Nothing Nothing time
            user2Key <- Db.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
            user3Key <- Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            categoryKey <- Db.createCategory "tutor" time
            currentUserOfferKey <- Db.createOffer currentUserKey categoryKey photoKey "chemistry" 1 time
            user2OfferKey <- Db.createOffer user2Key categoryKey photoKey "physics" 1 time
            user3OfferKey <- Db.createOffer user3Key categoryKey photoKey "calculus" 1 time
            trade1Key <- Db.createTrade currentUserOfferKey user2OfferKey False time
            tradeChat1Key <- Db.createTradeChat trade1Key time
            trade2Key <- Db.createTrade user3OfferKey currentUserOfferKey False time
            tradeChat2Key <- Db.createTradeChat trade2Key time
            trade3Key <- Db.createTrade user2OfferKey user3OfferKey False time
            tradeChat3Key <- Db.createTradeChat trade3Key time
            message1Key <-
              Db.createMessage tradeChat1Key currentUserKey "yo fred"
                (UTCTime (fromGregorian 2018 5 1) (secondsToDiffTime 0))
            message2Key <- Db.createMessage tradeChat2Key currentUserKey "yo bill" time
            message3Key <- Db.createMessage tradeChat3Key user2Key "hey bill, it's me fred." time
            message4Key <- Db.createMessage tradeChat1Key user2Key "hello, ed" time
            chatData <- findChatData currentUserKey
            expectedRes <- expected (tradeChat1Key, user2Key, [message4Key, message1Key]) (tradeChat2Key, user3Key, [message2Key])
            return (chatData, expectedRes)
          chatData `shouldBe` expectedRes
      context "destroyTradeChat" $
        it "removes the given trade chat from the database" $ \config -> do
          tradeChatCount <- Spec.runAppToIO config $ do
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
            destroyTradeChat tradeChatKey
            Db.run $ Sql.count ([] :: [Sql.Filter TradeChat])
          tradeChatCount `shouldBe` 0
