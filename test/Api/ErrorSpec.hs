module Api.ErrorSpec where

import Api.Error
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "ErrorSpec" $ do
        it "can take stringified SqlError and extract the details" $ do
            (parseSqlError sqlError) `shouldBe` (expected :: String)
  where
    expected =
        mconcat ["The username", " \"", "hamilton", "\" ", "already exists."]
    sqlError =
        "SqlError {sqlState = \"23505\", sqlExecStatus = FatalError, sqlErrorMsg = \"duplicate key value violates unique constraint \\\"unique_username\\\"\", sqlErrorDetail = \"Key (username)=(hamilton) already exists.\", sqlErrorHint = \"\"}"
