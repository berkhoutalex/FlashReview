{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Server( server, initializeApp, AppEnv(..) ) where

import qualified API                        (Flashcard (..), FlashcardAPI,
                                             ReviewResult, Stats (..),
                                             User (..))
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.UUID                  (UUID)
import qualified Database                   as DB
import qualified Database.PostgreSQL.Simple as PG
import           Servant
import           Servant.Auth.Server

data AppEnv = AppEnv
  { appDbConn         :: PG.Connection
  , appCookieSettings :: CookieSettings
  , appJWTSettings    :: JWTSettings
  }

initializeApp :: IO AppEnv
initializeApp = do
  conn <- DB.connectDb
  DB.setupSchema conn
  myKey <- generateKey
  let jwtSettings = defaultJWTSettings myKey
      cookieSettings = defaultCookieSettings

  pure $ AppEnv conn cookieSettings jwtSettings


server :: AppEnv -> Server (API.FlashcardAPI '[JWT])
server env =
       (`getCards` env)
  :<|> (`createCard` env)
  :<|> (`updateCard` env)
  :<|> (`deleteCard` env)
  :<|> (`getReviewQueue` env)
  :<|> (`submitReview` env)
  :<|> (`getStats` env)
  :<|> userLogin env
  :<|> userSignup env


getCards :: AuthResult API.User -> AppEnv -> Handler [API.Flashcard]
getCards authResult AppEnv{..} =
  case authResult of
    Authenticated user -> liftIO $ DB.getAllCardsDb appDbConn (API.userId user)
    _                  -> throwError err401

createCard :: AuthResult API.User -> AppEnv -> API.Flashcard -> Handler API.Flashcard
createCard authResult AppEnv{..} flashcard =
  case authResult of
    Authenticated user ->
      -- Set the ownerId field to the authenticated user's ID
      let cardWithOwner = flashcard { API.ownerId = API.userId user }
      in liftIO $ DB.createCardDb appDbConn cardWithOwner
    _               -> throwError err401

updateCard :: AuthResult API.User -> AppEnv -> UUID -> API.Flashcard -> Handler API.Flashcard
updateCard authResult AppEnv{..} uuid flashcard =
  case authResult of
    Authenticated user ->
      -- Ensure the card belongs to the authenticated user
      let cardWithOwner = flashcard { API.ownerId = API.userId user }
      in liftIO $ DB.updateCardDb appDbConn uuid cardWithOwner
    _               -> throwError err401

deleteCard :: AuthResult API.User -> AppEnv -> UUID -> Handler NoContent
deleteCard authResult AppEnv{..} uuid =
  case authResult of
    Authenticated user -> do
      liftIO $ DB.deleteCardDb appDbConn uuid (API.userId user)
      pure NoContent
    _ -> throwError err401

getReviewQueue :: AuthResult API.User -> AppEnv -> Handler [API.Flashcard]
getReviewQueue authResult AppEnv{..} =
  case authResult of
    Authenticated user -> liftIO $ DB.getReviewCardsDb appDbConn (API.userId user)
    _                  -> throwError err401

submitReview :: AuthResult API.User -> AppEnv -> UUID -> API.ReviewResult -> Handler NoContent
submitReview authResult AppEnv{..} uuid result =
  case authResult of
    Authenticated user -> do
      liftIO $ DB.processReviewDb appDbConn uuid (API.userId user) result
      pure NoContent
    _ -> throwError err401

getStats :: AuthResult API.User -> AppEnv -> Handler API.Stats
getStats authResult AppEnv{..} =
  case authResult of
    Authenticated user -> do
      dueCount <- liftIO $ DB.getDueCountDb appDbConn (API.userId user)
      pure $ API.Stats dueCount
    _ -> throwError err401

userLogin :: AppEnv -> API.User -> Handler (Headers '[Header "Set-Cookie" SetCookie,Header "Set-Cookie" SetCookie] String)
userLogin AppEnv{..} user = do
  mUser <- liftIO $ DB.authenticateUserDb appDbConn (API.username user) (API.password user)
  case mUser of
    Nothing -> throwError $ err401 {errBody = BSC.pack "email/password not found"}
    Just user -> do
        mLoginAccepted <- liftIO $ acceptLogin appCookieSettings appJWTSettings user
        case mLoginAccepted of
            Nothing -> throwError $ err401 {errBody = BSC.pack "login failed! Please try again!"}
            Just x -> do
                eJWT <- liftIO $ makeJWT user appJWTSettings Nothing
                case eJWT of
                    Left _ -> throwError $ err401 {errBody = BSC.pack "login failed! please try again!"}
                    Right r -> return $ x (BSC.unpack r)

userSignup :: AppEnv -> API.User -> Handler API.User
userSignup AppEnv{..} user = do
  liftIO $ DB.signupUserDb appDbConn user
