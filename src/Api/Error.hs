{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Error
    ( ApiErr(PasswordLengthLT6, PasswordConfirmationMismatch,
       InvalidOrMissingToken, AuthedUserNotFound, UserNotFound,
       InvalidCredentials, MissingAuthHeader, CustomError, RequestedUserNotAuth)
    , apiErr
    , sqlError
    , StatusCode(..)
    , parseSqlError
    ) where

import           Control.Applicative        (liftA2)
import           Data.Aeson                 (ToJSON, encode)
import           Data.Bifunctor             (bimap)
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.CaseInsensitive       (mk)
import           Data.Int                   (Int64)
import           Data.List                  (dropWhileEnd, isInfixOf)
import           Data.List.Utils            (split)
import           Data.Maybe                 (fromMaybe)
import           GHC.Generics               (Generic)
import qualified Servant                    as S
import           Utils                      (sHead)

data ApiErr
    = PasswordLengthLT6
    | PasswordConfirmationMismatch
    | InvalidOrMissingToken
    | AuthedUserNotFound
    | UserNotFound Int64
    | InvalidCredentials
    | MissingAuthHeader
    | SqlError String
    | RequestedUserNotAuth
    | CustomError String
    deriving (Show)

data StatusCode
    = E400
    | E401
    | E404
    | E500
    deriving (Generic, Show)

instance ToJSON StatusCode

data JsonError = JsonError
    { statusCode :: String
    , title      :: String
    , detail     :: String
    } deriving (Generic, Show)

instance ToJSON JsonError

apiErr :: (StatusCode, ApiErr) -> S.ServantErr
apiErr = encodeJsonError . jsonFromApiErr

encodeJsonError :: JsonError -> S.ServantErr
encodeJsonError jsonError =
    err {S.errBody = jsonBody, S.errHeaders = [jsonHeader]}
  where
    err = getErrorFromCode $ statusCode jsonError
    jsonBody = encode jsonError
    jsonHeader =
        ((mk $ pack "Content-Type"), (pack "application/json;charset=utf-8"))

jsonFromApiErr :: (StatusCode, ApiErr) -> JsonError
jsonFromApiErr (stCode, errType) =
    JsonError
    { statusCode = (tail $ show stCode)
    , title = (show errType)
    , detail = (msgFromErr errType)
    }

msgFromErr :: ApiErr -> String
msgFromErr err =
    case err of
        PasswordLengthLT6 -> "Password must be greater than 6 characters."
        PasswordConfirmationMismatch ->
            "Password does not match confirmation"
        InvalidOrMissingToken -> "Invalid or missing auth token."
        AuthedUserNotFound ->
            "Could not find user associated with given auth token."
        UserNotFound userId -> "Could not find user with id of " ++ (show userId)
        InvalidCredentials -> "Invalid username or password."
        MissingAuthHeader -> "Missing 'Authorization' header in request."
        RequestedUserNotAuth -> "User requested does not match user associated with provided auth token."
        SqlError sqlErr -> sqlErr
        CustomError msg -> msg

getErrorFromCode :: String -> S.ServantErr
getErrorFromCode stCode =
    case stCode of
        "400" -> S.err400
        "401" -> S.err401
        "404" -> S.err404
        "500" -> S.err500

sqlError :: String -> ApiErr
sqlError strErr = SqlError (parseSqlError strErr)

parseSqlError :: String -> String
parseSqlError strErr =
    let detailField = getDetailFieldFromSqlError strErr
    in fromMaybe strErr $
       liftA2
           buildMsg
           (detailField >>= getDataFromDetailField)
           (getDetailsFromDetailField <$> detailField)
  where
    buildMsg (field, value) details =
        mconcat ["The ", field, " \"", value, "\" ", details]

getDetailFieldFromSqlError :: String -> Maybe String
getDetailFieldFromSqlError =
    let dropHeaders = reverse . drop 1 . reverse . dropWhile (/= '(')
    in (dropHeaders <$>) .
       (sHead . filter (isInfixOf "sqlErrorDetail") . split ",")

getDataFromDetailField :: String -> Maybe (String, String)
getDataFromDetailField =
    let mkRawFields =
            (break (== '=') <$>) <$> (sHead . filter (elem '=') . words)
        mkReadable = foldr readableField ""
    in (bimap mkReadable mkReadable <$>) <$> mkRawFields

getDetailsFromDetailField :: String -> String
getDetailsFromDetailField =
    unwords . drop 1 . reverse . dropWhileEnd (not . elem '=') . reverse . words

readableField :: Char -> String -> String
readableField char acc
    | elem char ("(=)_" :: String) = acc
    | otherwise = char : acc
