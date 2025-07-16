{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Server( server, initializeApp, AppEnv(..) ) where

import qualified API                        (Flashcard (..), FlashcardAPI,
                                             FlashcardRequest (..),
                                             LoginRequest (loginPassword, loginUsername),
                                             ReviewResult, SignupRequest (..),
                                             Stats (..), User (..),
                                             UserJWT (..))
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.UUID                  (UUID)
import qualified Data.UUID.V4               (nextRandom)
import qualified Database                   as DB
import qualified Database.PostgreSQL.Simple as PG
import           Servant                    hiding (BadPassword, NoSuchUser)
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


  let cookieSettings = defaultCookieSettings {
        cookieIsSecure = NotSecure,
        cookieXsrfSetting = Nothing
      }

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



getCards :: AuthResult API.UserJWT -> AppEnv -> Handler [API.Flashcard]
getCards authResult AppEnv{..} =
  case authResult of
    Authenticated user -> do
      liftIO $ DB.getAllCardsDb appDbConn (API.userJwtId user)

    _ -> throwError err401

createCard :: AuthResult API.UserJWT -> AppEnv -> API.FlashcardRequest -> Handler API.Flashcard
createCard authResult AppEnv{..} flashcardReq =
  case authResult of
    Authenticated user ->

      let flashcard = API.Flashcard
            { API.id          = API.reqId flashcardReq
            , API.front       = API.reqFront flashcardReq
            , API.back        = API.reqBack flashcardReq
            , API.nextReview  = API.reqNextReview flashcardReq
            , API.interval    = API.reqInterval flashcardReq
            , API.easeFactor  = API.reqEaseFactor flashcardReq
            , API.repetitions = API.reqRepetitions flashcardReq
            , API.ownerId     = API.userJwtId user
            }
      in liftIO $ DB.createCardDb appDbConn flashcard
    _               -> throwError err401

updateCard :: AuthResult API.UserJWT -> AppEnv -> UUID -> API.FlashcardRequest -> Handler API.Flashcard
updateCard authResult AppEnv{..} uuid flashcardReq =
  case authResult of
    Authenticated user ->

      let flashcard = API.Flashcard
            { API.id          = uuid
            , API.front       = API.reqFront flashcardReq
            , API.back        = API.reqBack flashcardReq
            , API.nextReview  = API.reqNextReview flashcardReq
            , API.interval    = API.reqInterval flashcardReq
            , API.easeFactor  = API.reqEaseFactor flashcardReq
            , API.repetitions = API.reqRepetitions flashcardReq
            , API.ownerId     = API.userJwtId user
            }
      in liftIO $ DB.updateCardDb appDbConn uuid flashcard
    _               -> throwError err401

deleteCard :: AuthResult API.UserJWT -> AppEnv -> UUID -> Handler NoContent
deleteCard authResult AppEnv{..} uuid =
  case authResult of
    Authenticated user -> do
      liftIO $ DB.deleteCardDb appDbConn uuid (API.userJwtId user)
      pure NoContent
    _ -> throwError err401

getReviewQueue :: AuthResult API.UserJWT -> AppEnv -> Handler [API.Flashcard]
getReviewQueue authResult AppEnv{..} =
  case authResult of
    Authenticated user -> do
      liftIO $ DB.getReviewCardsDb appDbConn (API.userJwtId user)
    _ -> throwError err401

submitReview :: AuthResult API.UserJWT -> AppEnv -> UUID -> API.ReviewResult -> Handler NoContent
submitReview authResult AppEnv{..} uuid result =
  case authResult of
    Authenticated user -> do
      liftIO $ DB.processReviewDb appDbConn uuid (API.userJwtId user) result
      pure NoContent
    _ -> throwError err401

getStats :: AuthResult API.UserJWT -> AppEnv -> Handler API.Stats
getStats authResult AppEnv{..} =
  case authResult of
    Authenticated user -> do
      dueCount <- liftIO $ DB.getDueCountDb appDbConn (API.userJwtId user)
      pure $ API.Stats dueCount
    _ -> throwError err401

userLogin :: AppEnv -> API.LoginRequest -> Handler (Headers '[Header "Set-Cookie" SetCookie,Header "Set-Cookie" SetCookie] String)
userLogin AppEnv{..} loginReq = do
  mUser <- liftIO $ DB.authenticateUserDb appDbConn (API.loginUsername loginReq) (API.loginPassword loginReq)
  case mUser of
    Nothing -> do
      throwError $ err401 {errBody = BSC.pack "email/password not found"}
    Just user -> do

        let userJwt = API.UserJWT
              { userJwtId = API.userId user
              , userJwtName = API.username user
              , userJwtEmail = API.email user
              }
        mLoginAccepted <- liftIO $ acceptLogin appCookieSettings appJWTSettings userJwt
        case mLoginAccepted of
            Nothing -> do
              throwError $ err401 {errBody = BSC.pack "login failed! Please try again!"}
            Just x -> do

                eJWT <- liftIO $ makeJWT userJwt appJWTSettings Nothing
                case eJWT of
                    Left _ -> do
                      throwError $ err401 {errBody = BSC.pack "login failed! please try again!"}
                    Right r -> do

                      return $ x (BSC.unpack r)

userSignup :: AppEnv -> API.SignupRequest -> Handler API.User
userSignup AppEnv{..} signupReq = do
  userId <- liftIO Data.UUID.V4.nextRandom
  let user = API.User
        { API.userId = userId
        , API.username = API.signupUsername signupReq
        , API.email = API.signupEmail signupReq
        , API.password = API.signupPassword signupReq
        }
  liftIO $ DB.signupUserDb appDbConn user
