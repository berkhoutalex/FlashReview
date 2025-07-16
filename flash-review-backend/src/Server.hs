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

  -- Setup JWT settings
  let jwtSettings = defaultJWTSettings myKey

  -- Setup cookie settings for easier testing
  let cookieSettings = defaultCookieSettings {
        cookieIsSecure = NotSecure,  -- Allows non-HTTPS testing
        cookieXsrfSetting = Nothing   -- Disable XSRF for testing
      }

  putStrLn "Server starting with JWT authentication enabled"
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
      liftIO $ putStrLn $ "Authentication successful for user: " ++ show (API.userJwtId user)
      liftIO $ DB.getAllCardsDb appDbConn (API.userJwtId user)
    BadPassword -> do
      liftIO $ putStrLn "Authentication failed: Bad Password"
      throwError err401 { errBody = BSC.pack "Bad password provided" }
    NoSuchUser -> do
      liftIO $ putStrLn "Authentication failed: No Such User"
      throwError err401 { errBody = BSC.pack "User not found" }
    Indefinite -> do
      liftIO $ do
        putStrLn "\n=== JWT Authentication Debug Info ==="
        putStrLn "Authentication failed: Indefinite (token may be missing or malformed)"
        putStrLn "This usually means one of the following issues:"
        putStrLn " 1. No Authorization header is present in the request"
        putStrLn " 2. The Authorization header doesn't start with 'Bearer '"
        putStrLn " 3. The JWT token is malformed or invalid"
        putStrLn " 4. The JWT token has expired"
        putStrLn "=== End of Debug Info ===\n"

      throwError err401 { errBody = BSC.pack "Authorization token is missing or malformed. Make sure your request includes a valid 'Authorization: Bearer <token>' header." }

createCard :: AuthResult API.UserJWT -> AppEnv -> API.FlashcardRequest -> Handler API.Flashcard
createCard authResult AppEnv{..} flashcardReq =
  case authResult of
    Authenticated user ->
      -- Convert request to flashcard and set the ownerId field
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
      -- Convert request to flashcard and set the ownerId field
      let flashcard = API.Flashcard
            { API.id          = uuid -- Use the UUID from the URL parameter
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
      liftIO $ putStrLn $ "getReviewQueue: Auth successful for user: " ++ show (API.userJwtId user)
      liftIO $ DB.getReviewCardsDb appDbConn (API.userJwtId user)
    BadPassword -> do
      liftIO $ putStrLn "getReviewQueue: Authentication failed with BadPassword"
      throwError err401 { errBody = BSC.pack "Bad password" }
    NoSuchUser -> do
      liftIO $ putStrLn "getReviewQueue: Authentication failed with NoSuchUser"
      throwError err401 { errBody = BSC.pack "User not found" }
    Indefinite -> do
      liftIO $ putStrLn "getReviewQueue: Authentication failed with Indefinite (token may be missing or malformed)"
      throwError err401 { errBody = BSC.pack "Authorization token is missing or malformed" }

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
  liftIO $ putStrLn $ "Login attempt for username: " ++ show (API.loginUsername loginReq)
  mUser <- liftIO $ DB.authenticateUserDb appDbConn (API.loginUsername loginReq) (API.loginPassword loginReq)
  case mUser of
    Nothing -> do
      liftIO $ putStrLn "Login failed: User not found or password incorrect"
      throwError $ err401 {errBody = BSC.pack "email/password not found"}
    Just user -> do
        liftIO $ putStrLn $ "User authenticated successfully: " ++ show (API.username user)
        -- Create UserJWT instance for token (without password)
        let userJwt = API.UserJWT
              { userJwtId = API.userId user
              , userJwtName = API.username user
              , userJwtEmail = API.email user
              }
        liftIO $ putStrLn $ "Creating JWT for user: " ++ show (API.userJwtId userJwt)
        mLoginAccepted <- liftIO $ acceptLogin appCookieSettings appJWTSettings userJwt
        case mLoginAccepted of
            Nothing -> do
              liftIO $ putStrLn "Login failed: acceptLogin returned Nothing"
              throwError $ err401 {errBody = BSC.pack "login failed! Please try again!"}
            Just x -> do
                liftIO $ putStrLn "Cookies set successfully, generating JWT token"
                -- Use the User instance directly (ToJWT will convert to UserJWT)
                eJWT <- liftIO $ makeJWT userJwt appJWTSettings Nothing
                case eJWT of
                    Left err -> do
                      liftIO $ putStrLn $ "JWT creation failed with error: " ++ show err
                      throwError $ err401 {errBody = BSC.pack "login failed! please try again!"}
                    Right r -> do
                      liftIO $ do
                        putStrLn $ "JWT created successfully, length: " ++ show (BSC.length r)
                        putStrLn "\n=== CLIENT INTEGRATION INSTRUCTIONS ==="
                        putStrLn "For subsequent requests, include this JWT token in the Authorization header:"
                        putStrLn "Authorization: Bearer <token>"
                        putStrLn "Make sure to include the 'Bearer ' prefix exactly as shown."
                        putStrLn "=== END INSTRUCTIONS ===\n"
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
