{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Auth where

import qualified Control.Monad.Except             as BE
import           Data.Aeson                       (FromJSON, Result (..),
                                                   ToJSON, Value (String),
                                                   fromJSON)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as LBS
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)

import           Crypto.BCrypt                    (fastBcryptHashingPolicy,
                                                   hashPasswordUsingPolicy,
                                                   validatePassword)
import           Models                           (EntityField (UserUsername),
                                                   User, runDb, userPassword)

import           Api.Error                        (ApiErr (..), StatusCode (..),
                                                   apiErr)
import           Config                           (App, Config, getConfig,
                                                   getJwtSecret, runApp)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (runReaderT)
import           Data.Text.Encoding               (decodeUtf8)
import           Database.Persist.Postgresql      (Entity, entityVal,
                                                   selectFirst, (==.))
import           Network.Wai                      (Request, requestHeaders)
import           Servant                          ((:<|>), (:>), (:~>) (Nat),
                                                   AuthProtect, Handler, JSON,
                                                   Post, ReqBody, ServantErr,
                                                   ServerT, enter, err400,
                                                   err401, err404, throwError)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Web.JWT                          (Algorithm (HS256), JWT,
                                                   VerifiedJWT, claims, decode,
                                                   def, encodeSigned, iss,
                                                   secret, stringOrURI,
                                                   unregisteredClaims, verify)

data UserAuth = UserAuth
    { authName :: T.Text
    , token    :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON UserAuth

instance ToJSON UserAuth

data AuthEntry = AuthEntry
    { authUsername :: T.Text
    , authPassword :: T.Text
    } deriving (Show, Generic)

instance FromJSON AuthEntry

type instance AuthServerData (AuthProtect "jwt-auth") = User

type AuthAPI = "login" :> ReqBody '[ JSON] AuthEntry :> Post '[ JSON] UserAuth

authServer :: ServerT AuthAPI App
authServer = authUser

authHandler :: AuthHandler Request User
authHandler =
    let handler req =
            maybe
                (throwError $ apiErr (E401, MissingAuthHeader))
                validateAuthToken
                (lookup "Authorization" $ requestHeaders req)
    in mkAuthHandler handler

validateAuthToken :: BS.ByteString -> Servant.Handler User
validateAuthToken token = do
    jwtSecret <- liftIO (getJwtSecret <$> getConfig)
    maybe
        (throwError $ apiErr (E401, InvalidOrMissingToken))
        lookUpUser
        ((verifyToken jwtSecret) >>= maybeKeyFromToken)
  where
    (_, tkn) = T.breakOnEnd " " $ decodeUtf8 token
    verifyToken scrt = verify (secret $ T.pack scrt) =<< (decode tkn)
    maybeKeyFromToken scrt = getKeyFromToken (unregisteredClaims (claims scrt))

getKeyFromToken :: Map.Map T.Text Value -> Maybe String
getKeyFromToken cs =
    lookup (T.pack "name") (Map.toList cs) >>= \a ->
        case fromJSON a of
            Data.Aeson.Error s    -> Nothing
            Data.Aeson.Success st -> Just st

lookUpUser :: String -> Servant.Handler User
lookUpUser key = do
    cfg <- liftIO getConfig
    enter (convertAppx cfg) appUser
  where
    appUser = userFromDb key

convertAppx :: Config -> App :~> BE.ExceptT ServantErr IO
convertAppx cfg = Nat (flip runReaderT cfg . runApp)

userFromDb :: String -> App User
userFromDb str = do
    maybeUser <- runDb (selectFirst [UserUsername ==. str] [])
    maybe
        (throwError $ apiErr (E404, AuthedUserNotFound))
        (return . entityVal)
        maybeUser

encodePassword :: String -> IO (Maybe BS.ByteString)
encodePassword = hashPasswordUsingPolicy fastBcryptHashingPolicy . BS.pack

authUser :: AuthEntry -> App UserAuth
authUser authEntry = do
    jwtSecret <- liftIO (getJwtSecret <$> getConfig)
    maybeUser <- runDb (selectFirst [UserUsername ==. (T.unpack uName)] [])
    case maybeUser of
        Nothing -> throwError (apiErr (E404, UserNotFound))
        Just person
            | doPasswordsMatch authEntry person ->
                return $
                UserAuth
                {authName = uName, token = Just (tokenFromSecret jwtSecret)}
            | otherwise -> throwError $ apiErr (E400, InvalidPassword)
  where
    uName = authUsername authEntry
    cs =
        def
        { iss = stringOrURI "TradeLeaf"
        , unregisteredClaims = Map.fromList [("name", String $ uName)]
        }
    tokenFromSecret scrt = encodeSigned HS256 (secret $ T.pack scrt) cs

doPasswordsMatch :: AuthEntry -> Entity User -> Bool
doPasswordsMatch authEntry userToAuth =
    validatePassword
        (BS.pack $ userPassword $ entityVal userToAuth)
        (BS.pack $ T.unpack $ authPassword authEntry)

validatePasswordLength :: String -> Either ApiErr String
validatePasswordLength p =
    let l = length p
    in if l >= 6
           then Right p
           else Left PasswordLengthLT6

confirmPassword :: String -> String -> Either ApiErr String
confirmPassword p cp =
    if cp == p
        then Right p
        else Left PasswordConfirmationMismatch