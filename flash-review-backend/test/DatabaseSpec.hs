{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module DatabaseSpec (spec) where

import           API
import           Control.Exception          (bracket)
import           Data.Time.Clock            (getCurrentTime)
import           Data.UUID                  (UUID)
import qualified Data.UUID.V4               as UUID
import           Database
import qualified Database.PostgreSQL.Simple as PG
import           Prelude                    hiding (id)
import           Test.Hspec

testConfig :: DatabaseConfig
testConfig = DatabaseConfig
  { dbHost = "localhost"
  , dbPort = 5432
  , dbUser = "postgres"
  , dbPassword = "postgres"
  , dbDatabase = "flashcards_test"
  }

withTestConnection :: (PG.Connection -> IO a) -> IO a
withTestConnection action = do
  let connStr = makeConnectionString testConfig
  bracket (PG.connectPostgreSQL connStr) PG.close action


setupTestDb :: PG.Connection -> IO ()
setupTestDb conn = do

  _ <- PG.execute_ conn "DROP TABLE IF EXISTS flashcards CASCADE"
  _ <- PG.execute_ conn "DROP TABLE IF EXISTS users CASCADE"

  setupSchema conn


createTestUser :: PG.Connection -> IO User
createTestUser conn = do
  userId <- UUID.nextRandom
  let user = User
        { userId = userId
        , username =  "testuser"
        , email =  "test@example.com"
        , password =  "password"
        }
  signupUserDb conn user

createTestFlashcard :: UUID -> IO Flashcard
createTestFlashcard userId = do
  cardId <- UUID.nextRandom
  now <- Data.Time.Clock.getCurrentTime
  pure Flashcard
    { id = cardId
    , front =  "Test Front"
    , back =  "Test Back"
    , nextReview = now
    , interval = 1
    , easeFactor = 2.5
    , repetitions = 0
    , ownerId = userId
    }

spec :: Spec
spec = do
  around withTestConnection $ do
    describe "Database Operations" $ do
      it "should set up schema" $ \conn -> do
        setupTestDb conn
        [PG.Only n] <- PG.query_ conn "SELECT count(*) FROM information_schema.tables WHERE table_name = 'flashcards'"
        n `shouldBe` (1 :: Int)

        [PG.Only n'] <- PG.query_ conn "SELECT count(*) FROM information_schema.tables WHERE table_name = 'users'"
        n' `shouldBe` (1 :: Int)

      it "should create and authenticate user" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        mUser <- authenticateUserDb conn "testuser" "password"
        case mUser of
          Nothing -> expectationFailure "User authentication failed"
          Just u  -> userId u `shouldBe` userId user

        mUser' <- authenticateUserDb conn "testuser" "wrongpassword"
        mUser' `shouldBe` Nothing

      it "should create a flashcard" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        createdCard <- createCardDb conn card
        createdCard `shouldBe` card
        mCard <- getCardByIdDb conn (id card) (userId user)
        mCard `shouldBe` Just card

      it "should update a flashcard" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        _ <- createCardDb conn card

        let updatedCard = card { front =  "Updated Front", back =  "Updated Back" }


        mCard <- getCardByIdDb conn (id card) (userId user)
        mCard `shouldBe` Just updatedCard

      it "should delete a flashcard" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        _ <- createCardDb conn card

        deleteCardDb conn (id card) (userId user)
        mCard <- getCardByIdDb conn (id card) (userId user)
        mCard `shouldBe` Nothing

      it "should get review cards" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        _ <- createCardDb conn card
        cards <- getReviewCardsDb conn (userId user)
        length cards `shouldBe` 1

      it "should process a review" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        _ <- createCardDb conn card

        let review = ReviewResult { rating = 4 }
        processReviewDb conn (id card) (userId user) review

        mCard <- getCardByIdDb conn (id card) (userId user)
        case mCard of
          Nothing -> expectationFailure "Card not found after review"
          Just updatedCard -> do
            repetitions updatedCard `shouldBe` 1
            interval updatedCard `shouldBe` 6

      it "should get due count" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        _ <- createCardDb conn card

        count <- getDueCountDb conn (userId user)
        count `shouldBe` 1

        let review = ReviewResult { rating = 5 }
        processReviewDb conn (id card) (userId user) review

        count' <- getDueCountDb conn (userId user)
        count' `shouldBe` 0
