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
                                                     throwIO, try)
import           Control.Monad                      (void, when)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Data.Time                          (UTCTime, addUTCTime,
                                                     getCurrentTime)
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.ToRow   as PG
import           System.Environment                 (lookupEnv)

data DatabaseConfig = DatabaseConfig
  { dbHost     :: String
  , dbPort     :: Int
  , dbUser     :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show)

defaultConfig :: DatabaseConfig
defaultConfig = DatabaseConfig
  { dbHost     = "localhost"
  , dbPort     = 5432
  , dbUser     = "postgres"
  , dbPassword = "postgres"
  , dbDatabase = "flashcards"
  }

makeConnectionString :: DatabaseConfig -> ByteString
makeConnectionString DatabaseConfig{..} = BS.pack $
  "host=" <> dbHost <>
  " port=" <> show dbPort <>
  " user=" <> dbUser <>
  " password=" <> dbPassword <>
  " dbname=" <> dbDatabase <>
  " client_encoding=UTF8"


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

connectDb :: IO PG.Connection
connectDb = do
  putStrLn "Connecting to PostgreSQL database..."

  config <- loadConfig

  let connStr = makeConnectionString config


  connectionResult <- try (PG.connectPostgreSQL connStr)
  case connectionResult of
    Left (e :: SomeException) -> do
      throwIO e
    Right conn -> do
      putStrLn "Connected to PostgreSQL successfully!"
      return conn

withConnection :: (PG.Connection -> IO a) -> IO a
withConnection = bracket connectDb PG.close

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

data FlashcardRow = FlashcardRow
  { rowId          :: UUID
  , rowFront       :: Text
  , rowBack        :: Text
  , rowNextReview  :: UTCTime
  , rowInterval    :: Int
  , rowEaseFactor  :: Double
  , rowRepetitions :: Int
  } deriving (Show)

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

instance PG.FromRow FlashcardRow where
  fromRow = do
    uuid <- PG.field
    front <- PG.field
    back <- PG.field
    nextReview <- PG.field
    interval <- PG.field
    easeFactor <- PG.field
    FlashcardRow uuid front back nextReview interval easeFactor <$> PG.field

instance PG.ToRow FlashcardRow where
  toRow FlashcardRow{..} =
    [ PG.toField rowId
    , PG.toField rowFront
    , PG.toField rowBack
    , PG.toField rowNextReview
    , PG.toField rowInterval
    , PG.toField rowEaseFactor
    , PG.toField rowRepetitions
    ]

setupSchema :: PG.Connection -> IO ()
setupSchema conn = do
  putStrLn "Setting up database schema..."

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

  void $ PG.execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_flashcards_next_review ON flashcards (next_review)"

  putStrLn "Schema setup complete."

getAllCardsDb :: PG.Connection -> IO [API.Flashcard]
getAllCardsDb conn = do
  rows <- PG.query_ conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions \
    \FROM flashcards \
    \ORDER BY next_review ASC"
  return $ map fromRow rows

getCardByIdDb :: PG.Connection -> UUID -> IO (Maybe API.Flashcard)
getCardByIdDb conn uuid = do
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions \
    \FROM flashcards \
    \WHERE id = ?" [uuid]
  return $ case rows of
    [row] -> Just (fromRow row)
    _     -> Nothing

createCardDb :: PG.Connection -> API.Flashcard -> IO API.Flashcard
createCardDb conn card = do
  let row = toRow card
  void $ PG.execute conn
    "INSERT INTO flashcards (id, front, back, next_review, interval, ease_factor, repetitions) \
    \VALUES (?, ?, ?, ?, ?, ?, ?)" row
  return card

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
    void $ createCardDb conn card

  return card

deleteCardDb :: PG.Connection -> UUID -> IO ()
deleteCardDb conn uuid = do
  void $ PG.execute conn
    "DELETE FROM flashcards WHERE id = ?" [uuid]

getReviewCardsDb :: PG.Connection -> IO [API.Flashcard]
getReviewCardsDb conn = do
  now <- getCurrentTime
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions \
    \FROM flashcards \
    \WHERE next_review <= ? \
    \ORDER BY next_review ASC" [now]
  return $ map fromRow rows

processReviewDb :: PG.Connection -> UUID -> API.ReviewResult -> IO ()
processReviewDb conn uuid result = do
  maybeCard <- getCardByIdDb conn uuid
  case maybeCard of
    Nothing -> pure ()
    Just card -> do
      now <- getCurrentTime
      let newRepetitions = API.repetitions card + 1

          -- sm-2 algorithm for calculating new ease factor and interval
          newEaseFactor = max 1.3 $ API.easeFactor card +
                          (0.1 - (5 - fromIntegral (API.rating result)) *
                          (0.08 + (5 - fromIntegral (API.rating result)) * 0.02))

          newInterval =
            if API.rating result < 3
              then 1
              else case API.interval card of
                     1 -> 6
                     i -> round (fromIntegral i * newEaseFactor)

          newNextReview = addUTCTime (fromIntegral newInterval * 24 * 60 * 60) now

      void $ updateCardDb conn uuid card
        { API.interval = newInterval
        , API.easeFactor = newEaseFactor
        , API.repetitions = newRepetitions
        , API.nextReview = newNextReview
        }

getDueCountDb :: PG.Connection -> IO Int
getDueCountDb conn = do
  now <- getCurrentTime
  [PG.Only count] <- PG.query conn
    "SELECT COUNT(*) FROM flashcards WHERE next_review <= ?" [now]
  return count
