{-# LANGUAGE ScopedTypeVariables #-}

module Api.MessageSpec where

import Config (App)
import Api.Message (MessageRequest(..), createMessage, getMessages)
import Api.Error (apiErr, StatusCode(E401), ApiErr(Unauthorized))
import qualified Database.Persist.Sql as Sql
import qualified Db.Main as Db
import qualified SpecHelper as Spec
import Test.Hspec
import Test.QuickCheck
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Models.Message
import Data.Maybe (fromJust)
import Models.TradeChat

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Api.Message" $
      let
        currentUser time =
          Db.newUser "fred" "johnson" "fred@gmail.com" "freddy" "password" Nothing Nothing time
        uninvolvedUser time =
          Db.newUser "john" "unwelcome" "j@yahoo.com" "junwelcom" "password" Nothing Nothing time
      in do
      context "createMessage" $ do
        it "can create a new message" $ \config -> do
          (msgCount, mbMsg, tradeChatKey, senderKey, tradeChat, createTime) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Db.createCategory "tutoring" time
            photoKey <- Db.createPhoto "dog.png" time
            currentUserKey <- Db.run $ Sql.insert (currentUser time)
            user2Key <- Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            user1Offer <- Db.createOffer currentUserKey categoryKey photoKey "math" 99 time
            user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
            tradeKey <- Db.createTrade user1Offer user2Offer False time
            tradeChatKey <- Db.createTradeChat tradeKey time
            msgKey <- createMessage (MessageRequest (Sql.fromSqlKey tradeChatKey) "hey, bill") (currentUser time)
            (mbMsg :: Maybe Message) <- Db.run $ Sql.get (Sql.toSqlKey msgKey)
            msgCount <- Db.run $ Sql.count ([] :: [Sql.Filter Message])
            tradeChat <- fromJust <$> Db.run (Sql.get tradeChatKey) :: App TradeChat
            return (msgCount, mbMsg, tradeChatKey, currentUserKey, tradeChat, time)
          msgCount `shouldBe` 1
          messageTradeChatId <$> mbMsg `shouldBe` Just tradeChatKey
          messageSenderId <$> mbMsg `shouldBe` Just senderKey
          messageContent <$> mbMsg `shouldBe` Just "hey, bill"
          (tradeChatUpdatedAt tradeChat > createTime) `shouldBe` True
        it "returns an error if current user isn't involved in trade chat " $ \config -> do
          (tradeChatId, badUser) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Db.createCategory "tutoring" time
            photoKey <- Db.createPhoto "dog.png" time
            currentUserKey <- Db.run $ Sql.insert (currentUser time)
            user2Key <-
              Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            uninvolvedUserKey <- Db.run $ Sql.insert (uninvolvedUser time)
            user1Offer <- Db.createOffer currentUserKey categoryKey photoKey "math" 99 time
            user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
            tradeKey <- Db.createTrade user1Offer user2Offer False time
            tradeChatKey <- Db.createTradeChat tradeKey time
            return (Sql.fromSqlKey tradeChatKey, uninvolvedUser time)
          Spec.runAppToIO config (createMessage (MessageRequest tradeChatId "hey, bill") badUser) `shouldThrow` (== apiErr (E401, Unauthorized))
      context "getMessages" $ do
        it "gets all messages for given trade chat" $ \config -> do
          messages <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Db.createCategory "tutoring" time
            photoKey <- Db.createPhoto "dog.png" time
            currentUserKey <- Db.run $ Sql.insert (currentUser time)
            user2Key <-
              Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            user1Offer <- Db.createOffer currentUserKey categoryKey photoKey "math" 99 time
            user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
            tradeKey <- Db.createTrade user1Offer user2Offer False time
            tradeChatKey <- Db.createTradeChat tradeKey time
            msg1Key <- Db.createMessage tradeChatKey currentUserKey "hey bill" time
            msg2Key <- Db.createMessage tradeChatKey user2Key "hi fred" time
            msg3Key <-
              Db.createMessage tradeChatKey currentUserKey "whatcha doin?" time
            getMessages (Sql.fromSqlKey tradeChatKey) (currentUser time)
          messageContent . Sql.entityVal <$> messages `shouldMatchList` ["hey bill", "hi fred", "whatcha doin?"]
        it "returns a 401 if trade chat does not involve given user" $ \config -> do
          (tradeChatId, uninvolvedUser) <- Spec.runAppToIO config $ do
            time <- liftIO getCurrentTime
            categoryKey <- Db.createCategory "tutoring" time
            photoKey <- Db.createPhoto "dog.png" time
            currentUserKey <- Db.run $ Sql.insert (currentUser time)
            user2Key <-
              Db.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
            uninvolvedUserKey <- Db.run $ Sql.insert (uninvolvedUser time)
            user1Offer <- Db.createOffer currentUserKey categoryKey photoKey "math" 99 time
            user2Offer <- Db.createOffer user2Key categoryKey photoKey "physics" 99 time
            tradeKey <- Db.createTrade user1Offer user2Offer False time
            tradeChatKey <- Db.createTradeChat tradeKey time
            msg1Key <- Db.createMessage tradeChatKey currentUserKey "hey bill" time
            msg2Key <- Db.createMessage tradeChatKey user2Key "hi fred" time
            msg3Key <-
              Db.createMessage tradeChatKey currentUserKey "whatcha doin?" time
            return (Sql.fromSqlKey tradeChatKey, uninvolvedUser time)
          Spec.runAppToIO config (getMessages tradeChatId uninvolvedUser) `shouldThrow` (== apiErr (E401, Unauthorized))

