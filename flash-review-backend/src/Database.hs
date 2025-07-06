{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database
  ( connectDb
  , withConnection
  , withTransaction
  , DatabaseConfig(..)
  , PG.Connection
  , defaultConfig
  , getAllCardsDb
  , getCardByIdDb
  , createCardDb
  , updateCardDb
  , deleteCardDb
  , getReviewCardsDb
  , processReviewDb
  , getDueCountDb
  , setupSchema
  ) where

import qualified API
import           Control.Exception                  (SomeException, bracket,
                                                     catch, throwIO, try)
import           Control.Monad                      (void, when)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import           Data.Time                          (UTCTime, addUTCTime,
                                                     getCurrentTime)
import           Data.UUID                          (UUID)
import qualified Data.UUID                          as UUID
import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.ToRow   as PG
import           System.Environment                 (lookupEnv)
import qualified UnliftIO.Exception                 as UIO

-- Database configuration
data DatabaseConfig = DatabaseConfig
  { dbHost     :: String
  , dbPort     :: Int
  , dbUser     :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show)

-- Default database configuration
defaultConfig :: DatabaseConfig
defaultConfig = DatabaseConfig
  { dbHost     = "localhost"
  , dbPort     = 5432
  , dbUser     = "postgres"
  , dbPassword = "postgres"
  , dbDatabase = "flashcards"
  }

-- Create a database connection string
makeConnectionString :: DatabaseConfig -> ByteString
makeConnectionString DatabaseConfig{..} = BS.pack $
  "host=" <> dbHost <>
  " port=" <> show dbPort <>
  " user=" <> dbUser <>
  " password=" <> dbPassword <>
  " dbname=" <> dbDatabase <>
  " client_encoding=UTF8"

-- Load database configuration from environment variables
loadConfig :: IO DatabaseConfig
loadConfig = do
  host <- fromMaybe (dbHost defaultConfig) <$> lookupEnv "PGHOST"
  port <- maybe (dbPort defaultConfig) read <$> lookupEnv "PGPORT"
  user <- fromMaybe (dbUser defaultConfig) <$> lookupEnv "PGUSER"
  password <- fromMaybe (dbPassword defaultConfig) <$> lookupEnv "PGPASSWORD"
  database <- fromMaybe (dbDatabase defaultConfig) <$> lookupEnv "PGDATABASE"

  pure $ DatabaseConfig
    { dbHost = host
    , dbPort = port
    , dbUser = user
    , dbPassword = password
    , dbDatabase = database
    }

-- Connect to the database
connectDb :: IO PG.Connection
connectDb = do
  putStrLn "Connecting to PostgreSQL database..."

  -- Load config from environment
  config <- loadConfig

  let connStr = makeConnectionString config

  -- Log connection details (excluding password)
  putStrLn $ "Connecting to PostgreSQL at " <> dbHost config <> ":" <> show (dbPort config)
          <> " (db: " <> dbDatabase config <> ", user: " <> dbUser config <> ")"

  -- Attempt connection with error handling
  connectionResult <- try (PG.connectPostgreSQL connStr)
  case connectionResult of
    Left (e :: SomeException) -> do
      putStrLn $ "Database connection failed: " <> show e
      putStrLn "\nTroubleshooting tips:"
      putStrLn "1. Check if PostgreSQL is running (docker ps | Select-String postgres)"
      putStrLn "2. Check your credentials and database name"
      putStrLn "3. Try connecting directly (e.g., with psql)"
      putStrLn "4. See DOCKER-TROUBLESHOOTING.md for more help\n"
      throwIO e
    Right conn -> do
      putStrLn "Connected to PostgreSQL successfully!"
      return conn

-- Helper to run actions with a connection
withConnection :: (PG.Connection -> IO a) -> IO a
withConnection = bracket connectDb PG.close

-- Helper to run actions in a transaction
withTransaction :: PG.Connection -> (PG.Connection -> IO a) -> IO a
withTransaction conn action = do
  PG.begin conn
  result <- try (action conn)
  case result of
    Left (e :: SomeException) -> do
      PG.rollback conn
      throwIO e
    Right r -> do
      PG.commit conn
      return r

-- Flashcard type for database operations
data FlashcardRow = FlashcardRow
  { rowId          :: UUID
  , rowFront       :: Text
  , rowBack        :: Text
  , rowNextReview  :: UTCTime
  , rowInterval    :: Int
  , rowEaseFactor  :: Double
  , rowRepetitions :: Int
  } deriving (Show)

-- Convert from DB row to API Flashcard
fromRow :: FlashcardRow -> API.Flashcard
fromRow FlashcardRow{..} = API.Flashcard
  { API.id          = rowId
  , API.front       = rowFront
  , API.back        = rowBack
  , API.nextReview  = rowNextReview
  , API.interval    = rowInterval
  , API.easeFactor  = rowEaseFactor
  , API.repetitions = rowRepetitions
  }

-- Convert from API Flashcard to DB row
toRow :: API.Flashcard -> FlashcardRow
toRow card = FlashcardRow
  { rowId          = API.id card
  , rowFront       = API.front card
  , rowBack        = API.back card
  , rowNextReview  = API.nextReview card
  , rowInterval    = API.interval card
  , rowEaseFactor  = API.easeFactor card
  , rowRepetitions = API.repetitions card
  }

-- PostgreSQL FromRow instance
instance PG.FromRow FlashcardRow where
  fromRow = do
    uuid <- PG.field        -- postgresql-simple has a FromField instance for UUID
    front <- PG.field
    back <- PG.field
    nextReview <- PG.field
    interval <- PG.field
    easeFactor <- PG.field
    repetitions <- PG.field
    return $ FlashcardRow uuid front back nextReview interval easeFactor repetitions

-- PostgreSQL ToRow instance
instance PG.ToRow FlashcardRow where
  toRow FlashcardRow{..} =
    [ PG.toField rowId  -- Use UUID directly, postgresql-simple has a ToField instance for UUID
    , PG.toField rowFront
    , PG.toField rowBack
    , PG.toField rowNextReview
    , PG.toField rowInterval
    , PG.toField rowEaseFactor
    , PG.toField rowRepetitions
    ]

-- Create the schema
setupSchema :: PG.Connection -> IO ()
setupSchema conn = do
  putStrLn "Setting up database schema..."

  -- Create flashcards table if it doesn't exist
  void $ PG.execute_ conn
    "CREATE TABLE IF NOT EXISTS flashcards (\
    \  id UUID PRIMARY KEY, \
    \  front TEXT NOT NULL, \
    \  back TEXT NOT NULL, \
    \  next_review TIMESTAMP WITH TIME ZONE NOT NULL, \
    \  interval INTEGER NOT NULL, \
    \  ease_factor DOUBLE PRECISION NOT NULL, \
    \  repetitions INTEGER NOT NULL \
    \)"

  -- Create index on next_review for efficient querying
  void $ PG.execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_flashcards_next_review ON flashcards (next_review)"

  putStrLn "Schema setup complete."

-- Database operations

-- Get all flashcards
getAllCardsDb :: PG.Connection -> IO [API.Flashcard]
getAllCardsDb conn = do
  rows <- PG.query_ conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions \
    \FROM flashcards \
    \ORDER BY next_review ASC"
  return $ map fromRow rows

-- Get a flashcard by ID
getCardByIdDb :: PG.Connection -> UUID -> IO (Maybe API.Flashcard)
getCardByIdDb conn uuid = do
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions \
    \FROM flashcards \
    \WHERE id = ?" [uuid]
  return $ case rows of
    [row] -> Just (fromRow row)
    _     -> Nothing

-- Create a new flashcard
createCardDb :: PG.Connection -> API.Flashcard -> IO API.Flashcard
createCardDb conn card = do
  let row = toRow card
  void $ PG.execute conn
    "INSERT INTO flashcards (id, front, back, next_review, interval, ease_factor, repetitions) \
    \VALUES (?, ?, ?, ?, ?, ?, ?)" row
  return card

-- Update a flashcard
updateCardDb :: PG.Connection -> UUID -> API.Flashcard -> IO API.Flashcard
updateCardDb conn uuid card = do
  let row = toRow card
  rowsAffected <- PG.execute conn
    "UPDATE flashcards \
    \SET front = ?, back = ?, next_review = ?, interval = ?, ease_factor = ?, repetitions = ? \
    \WHERE id = ?"
    (rowFront row, rowBack row, rowNextReview row, rowInterval row,
     rowEaseFactor row, rowRepetitions row, uuid)

  when (rowsAffected == 0) $ do
    -- Card not found, insert it
    void $ createCardDb conn card

  return card

-- Delete a flashcard
deleteCardDb :: PG.Connection -> UUID -> IO ()
deleteCardDb conn uuid = do
  void $ PG.execute conn
    "DELETE FROM flashcards WHERE id = ?" [uuid]

-- Get cards due for review
getReviewCardsDb :: PG.Connection -> IO [API.Flashcard]
getReviewCardsDb conn = do
  now <- getCurrentTime
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions \
    \FROM flashcards \
    \WHERE next_review <= ? \
    \ORDER BY next_review ASC" [now]
  return $ map fromRow rows

-- Process a review for a flashcard
processReviewDb :: PG.Connection -> UUID -> API.ReviewResult -> IO ()
processReviewDb conn uuid result = do
  maybeCard <- getCardByIdDb conn uuid
  case maybeCard of
    Nothing -> pure ()
    Just card -> do
      now <- getCurrentTime
      let newRepetitions = API.repetitions card + 1

          -- This is a simplified spaced repetition algorithm (SM-2)
          -- Adjust easeFactor based on performance
          newEaseFactor = max 1.3 $ API.easeFactor card +
                          (0.1 - (5 - fromIntegral (API.rating result)) *
                          (0.08 + (5 - fromIntegral (API.rating result)) * 0.02))

          -- Calculate new interval based on rating
          newInterval =
            if API.rating result < 3
              then 1  -- If rating is < 3, reset to 1 day
              else case API.interval card of
                     1 -> 6   -- First successful review: 6 days
                     i -> round (fromIntegral i * newEaseFactor)

          -- Calculate next review date
          newNextReview = addUTCTime (fromIntegral newInterval * 24 * 60 * 60) now

      void $ updateCardDb conn uuid card
        { API.interval = newInterval
        , API.easeFactor = newEaseFactor
        , API.repetitions = newRepetitions
        , API.nextReview = newNextReview
        }

-- Get count of cards due for review
getDueCountDb :: PG.Connection -> IO Int
getDueCountDb conn = do
  now <- getCurrentTime
  [PG.Only count] <- PG.query conn
    "SELECT COUNT(*) FROM flashcards WHERE next_review <= ?" [now]
  return count
