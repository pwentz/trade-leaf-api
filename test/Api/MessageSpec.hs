{-# LANGUAGE ScopedTypeVariables #-}

module Api.MessageSpec where

import Api.Message (MessageRequest(..), createMessage)
import qualified Database.Persist.Sql as Sql
import qualified Db.Main as Db
import qualified SpecHelper as Spec
import Test.Hspec
import Test.QuickCheck
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Models.Message

spec :: Spec
spec =
  around Spec.setupTeardown $
    describe "Api.Message" $
      it "can create a new message" $ \config -> do
        (msgCount, mbMsg, tradeChatKey, senderKey) <- Spec.runAppToIO config $ do
          time <- liftIO getCurrentTime
          categoryKey <- Spec.createCategory "tutoring" time
          photoKey <- Spec.createPhoto "dog.png" time
          user1Key <- Spec.createUser "fred" "johnson" "fred@gmail.com" "fredd" "password" Nothing Nothing time
          user2Key <- Spec.createUser "bill" "johnson" "bill@gmail.com" "billy" "password" Nothing Nothing time
          user1Offer <- Spec.createOffer user1Key categoryKey photoKey "math" 99 time
          user2Offer <- Spec.createOffer user2Key categoryKey photoKey "physics" 99 time
          tradeKey <- Spec.createTrade user1Offer user2Offer False time
          tradeChatKey <- Spec.createTradeChat tradeKey time
          msgKey <- createMessage (MessageRequest (Sql.fromSqlKey tradeChatKey) (Sql.fromSqlKey user1Key) "hey, bill")
          (mbMsg :: Maybe Message) <- Db.run $ Sql.get (Sql.toSqlKey msgKey)
          msgCount <- Db.run $ Sql.count ([] :: [Sql.Filter Message])
          return (msgCount, mbMsg, tradeChatKey, user1Key)
        msgCount `shouldBe` 1
        messageTradeChatId <$> mbMsg `shouldBe` Just tradeChatKey
        messageSenderId <$> mbMsg `shouldBe` Just senderKey
        messageContent <$> mbMsg `shouldBe` Just "hey, bill"

