{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module ServerSpec (spec) where

import           Data.Aeson                  (encode, object, (.=))
import           Data.Aeson.Key              (Key, fromString)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Lazy.Char8  as BSL8
import qualified Data.CaseInsensitive        as CI
import qualified Data.List                   as List
import           Data.Time.Clock             (getCurrentTime)
import qualified Data.UUID.V4                as UUID
import qualified Database.PostgreSQL.Simple  as PG
import           Network.HTTP.Types
import           Test.Hspec
import           Test.Hspec.Wai

import           API
import           Database                    (DatabaseConfig (..))
import qualified Database                    as DB
import           Server                      (AppEnv (..), server)

import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.Auth.Server

key :: String -> Data.Aeson.Key.Key
key = fromString

bodyContains :: String -> MatchBody
bodyContains s = MatchBody $ \_ body ->
  let bodyStr = BSL8.unpack body
  in if List.isInfixOf s bodyStr
     then Nothing
     else Just $ "Expected body to contain: " ++ s

-- Helper for matching any value in header tests

testConfig :: DatabaseConfig
testConfig = DatabaseConfig
  { dbHost = "localhost"
  , dbPort = 5432
  , dbUser = "postgres"
  , dbPassword = "postgres"
  , dbDatabase = "flashcards_test"
  }

makeTestApp :: IO Application
makeTestApp = do
  let connStr = DB.makeConnectionString testConfig
  conn <- PG.connectPostgreSQL connStr

  DB.setupSchema conn

  userId <- UUID.nextRandom
  let user = User
        { userId = userId
        , username =  "testuser"
        , email =  "test@example.com"
        , password =  "password"
        }
  _ <- DB.signupUserDb conn user

  myKey <- generateKey
  let jwtSettings = defaultJWTSettings myKey
  let cookieSettings = defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieXsrfSetting = Nothing
        }

  let env = AppEnv conn cookieSettings jwtSettings
  let corsPolicy = simpleCorsResourcePolicy
        { corsRequestHeaders = [hContentType, hAuthorization]
        , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
        }

  return $ cors (const $ Just corsPolicy) $ serveWithContext
    (Proxy :: Proxy (FlashcardAPI '[JWT]))
    (cookieSettings :. jwtSettings :. EmptyContext)
    (server env)

getTestToken :: IO BSL.ByteString
getTestToken = do
  let connStr = DB.makeConnectionString testConfig
  conn <- PG.connectPostgreSQL connStr

  mUser <- DB.authenticateUserDb conn "testuser" "password"
  case mUser of
    Nothing -> error "Test user authentication failed"
    Just user -> do
      myKey <- generateKey
      let jwtSettings = defaultJWTSettings myKey

      let userJwt = UserJWT
            { userJwtId = userId user
            , userJwtName = username user
            , userJwtEmail = email user
            }

      etoken <- makeJWT userJwt jwtSettings Nothing
      case etoken of
        Left err    -> error $ "Failed to create JWT token: " ++ show err
        Right token -> return token

spec :: Spec
spec = do
  describe "Server API" $ do
    with makeTestApp $ do
      describe "Authentication" $ do
        it "should reject unauthenticated requests" $ do
          get (BS8.pack "/cards") `shouldRespondWith` 401

        it "should allow signup" $ do
          let signupReq = encode $ object
                [ key "username" .= ("newuser" :: String)
                , key "email" .= ("newuser@example.com" :: String)
                , key "password" .= ("password" :: String)
                ]

          post (BS8.pack "/signup") signupReq
            `shouldRespondWith` 200

        it "should allow login" $ do
          let loginReq = encode $ object
                [ key "username" .= ("testuser" :: String)
                , key "password" .= ("password" :: String)
                ]

          post (BS8.pack "/login") loginReq
            `shouldRespondWith` 200

      describe "Flashcard Operations" $ do
        it "should create and retrieve flashcards" $ do
          token <- liftIO getTestToken
          uuid <- liftIO UUID.nextRandom
          now <- liftIO getCurrentTime
          let cardReq = encode $ object
                [ key "id" .= show uuid
                , key "front" .= ("Test Front" :: String)
                , key "back" .= ("Test Back" :: String)
                , key "nextReview" .= show now
                , key "interval" .= (1 :: Int)
                , key "easeFactor" .= (2.5 :: Double)
                , key "repetitions" .= (0 :: Int)
                ]

          request methodPost (BS8.pack "/cards")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            cardReq
            `shouldRespondWith` 200
          request methodGet (BS8.pack "/cards")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            BSL8.empty
            `shouldRespondWith` 200 { matchBody = bodyContains "Test Front" }

        it "should update flashcards" $ do
          token <- liftIO getTestToken
          uuid <- liftIO UUID.nextRandom
          now <- liftIO getCurrentTime
          let cardReq = encode $ object
                [ key "id" .= show uuid
                , key "front" .= ("Test Front" :: String)
                , key "back" .= ("Test Back" :: String)
                , key "nextReview" .= show now
                , key "interval" .= (1 :: Int)
                , key "easeFactor" .= (2.5 :: Double)
                , key "repetitions" .= (0 :: Int)
                ]

          request methodPost (BS8.pack "/cards")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            cardReq
            `shouldRespondWith` 200

          let updateReq = encode $ object
                [ key "id" .= show uuid
                , key "front" .= ("Updated Front" :: String)
                , key "back" .= ("Updated Back" :: String)
                , key "nextReview" .= show now
                , key "interval" .= (1 :: Int)
                , key "easeFactor" .= (2.5 :: Double)
                , key "repetitions" .= (0 :: Int)
                ]

          request methodPut (BS8.pack "/cards/" <> BS8.pack (show uuid))
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            updateReq
            `shouldRespondWith` 200 { matchBody = bodyContains "Updated Front" }

        it "should delete flashcards" $ do
          token <- liftIO getTestToken
          uuid <- liftIO UUID.nextRandom
          now <- liftIO getCurrentTime

          let cardReq = encode $ object
                [ key "id" .= show uuid
                , key "front" .= ("Test Front" :: String)
                , key "back" .= ("Test Back" :: String)
                , key "nextReview" .= show now
                , key "interval" .= (1 :: Int)
                , key "easeFactor" .= (2.5 :: Double)
                , key "repetitions" .= (0 :: Int)
                ]

          request methodPost (BS8.pack "/cards")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            cardReq
            `shouldRespondWith` 200

          request methodDelete (BS8.pack "/cards/" <> BS8.pack (show uuid))
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            BSL8.empty
            `shouldRespondWith` 204

        it "should get review queue" $ do
          token <- liftIO getTestToken

          request methodGet (BS8.pack "/review/queue")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            BSL8.empty
            `shouldRespondWith` 200

        it "should submit review results" $ do
          token <- liftIO getTestToken
          uuid <- liftIO UUID.nextRandom
          now <- liftIO getCurrentTime

          let cardReq = encode $ object
                [ key "id" .= show uuid
                , key "front" .= ("Test Front" :: String)
                , key "back" .= ("Test Back" :: String)
                , key "nextReview" .= show now
                , key "interval" .= (1 :: Int)
                , key "easeFactor" .= (2.5 :: Double)
                , key "repetitions" .= (0 :: Int)
                ]

          request methodPost (BS8.pack "/cards")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            cardReq
            `shouldRespondWith` 200

          let reviewReq = encode $ object
                [ key "rating" .= (4 :: Int)
                ]

          request methodPost (BS8.pack "/review/" <> BS8.pack (show uuid))
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            reviewReq
            `shouldRespondWith` 204

        it "should get stats" $ do
          token <- liftIO getTestToken

          request methodGet (BS8.pack "/stats")
            [(CI.mk $ BS8.pack "Authorization", BS8.pack "Bearer " <> BSL.toStrict token)]
            BSL8.empty
            `shouldRespondWith` 200 { matchBody = bodyContains "dueToday" }
