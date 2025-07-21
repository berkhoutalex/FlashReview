
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
  , authenticateUserDb
  , signupUserDb
  , makeConnectionString
  ) where

import qualified API
import           Control.Exception                  (SomeException, bracket,
                                                     throwIO, try)
import           Control.Monad                      (void, when)
import           Crypto.BCrypt                      (hashPasswordUsingPolicy,
                                                     slowerBcryptHashingPolicy,
                                                     validatePassword)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as TE
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
  , rowUserId      :: UUID
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
  , API.ownerId     = rowUserId
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
  , rowUserId      = API.ownerId card
  }

instance PG.FromRow FlashcardRow where
  fromRow = do
    uuid <- PG.field
    front <- PG.field
    back <- PG.field
    nextReview <- PG.field
    interval <- PG.field
    easeFactor <- PG.field
    repetitions <- PG.field
    FlashcardRow uuid front back nextReview interval easeFactor repetitions <$> PG.field

instance PG.ToRow FlashcardRow where
  toRow FlashcardRow{..} =
    [ PG.toField rowId
    , PG.toField rowFront
    , PG.toField rowBack
    , PG.toField rowNextReview
    , PG.toField rowInterval
    , PG.toField rowEaseFactor
    , PG.toField rowRepetitions
    , PG.toField rowUserId
    ]

data UserRow = UserRow
  { userId       :: UUID
  , userName     :: Text
  , userPassword :: Text
  , userEmail    :: Text
  } deriving (Show)

fromUserRow :: UserRow -> API.User
fromUserRow UserRow{..} = API.User
  { API.userId = userId
  , API.username = userName
  , API.email = userEmail
  , API.password = userPassword
  }

toUserRow :: API.User -> UserRow
toUserRow API.User{..} = UserRow
  { userId = userId
  , userName = username
  , userPassword = password
  , userEmail = email
  }

instance PG.FromRow UserRow where
  fromRow = do
    userId <- PG.field
    userName <- PG.field
    userPassword <- PG.field
    userEmail <- PG.field
    return UserRow{..}

instance PG.ToRow UserRow where
  toRow UserRow{..} =
    [ PG.toField userId
    , PG.toField userName
    , PG.toField userPassword
    , PG.toField userEmail
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
    \  repetitions INTEGER NOT NULL, \
    \  user_id UUID NOT NULL \
    \)"

  void $ PG.execute_ conn
    "CREATE TABLE IF NOT EXISTS users (\
    \  userid UUID PRIMARY KEY, \
    \  username TEXT NOT NULL UNIQUE, \
    \  password TEXT NOT NULL, \
    \  email TEXT NOT NULL UNIQUE \
    \)"

  void $ PG.execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_flashcards_next_review ON flashcards (next_review)"

  void $ PG.execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_flashcards_user_id ON flashcards (user_id)"

  void $ PG.execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF NOT EXISTS ( \
    \    SELECT 1 FROM information_schema.table_constraints \
    \    WHERE constraint_name = 'fk_flashcards_user_id' \
    \  ) THEN \
    \    ALTER TABLE flashcards \
    \    ADD CONSTRAINT fk_flashcards_user_id \
    \    FOREIGN KEY (user_id) \
    \    REFERENCES users(userid); \
    \  END IF; \
    \END; \
    \$$"


  void $ PG.execute_ conn
    "DO $$ \
    \BEGIN \
    \  IF NOT EXISTS ( \
    \    SELECT 1 FROM information_schema.columns \
    \    WHERE table_name = 'flashcards' AND column_name = 'user_id' \
    \  ) THEN \
    \    ALTER TABLE flashcards ADD COLUMN user_id UUID; \
    \  END IF; \
    \END; \
    \$$"

  putStrLn "Schema setup complete."

getAllCardsDb :: PG.Connection -> UUID -> IO [API.Flashcard]
getAllCardsDb conn userId = do
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions, user_id \
    \FROM flashcards \
    \WHERE user_id = ? \
    \ORDER BY next_review ASC" [userId]
  return $ map fromRow rows

getCardByIdDb :: PG.Connection -> UUID -> UUID -> IO (Maybe API.Flashcard)
getCardByIdDb conn uuid userId = do
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions, user_id \
    \FROM flashcards \
    \WHERE id = ? AND user_id = ?" [uuid, userId]
  return $ case rows of
    [row] -> Just (fromRow row)
    _     -> Nothing

createCardDb :: PG.Connection -> API.Flashcard -> IO API.Flashcard
createCardDb conn card = do
  let row = toRow card
  void $ PG.execute conn
    "INSERT INTO flashcards (id, front, back, next_review, interval, ease_factor, repetitions, user_id) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?)" row
  return card

updateCardDb :: PG.Connection -> UUID -> API.Flashcard -> IO API.Flashcard
updateCardDb conn uuid card = do
  let row = toRow card
  rowsAffected <- PG.execute conn
    "UPDATE flashcards \
    \SET front = ?, back = ?, next_review = ?, interval = ?, ease_factor = ?, repetitions = ? \
    \WHERE id = ? AND user_id = ?"
    (rowFront row, rowBack row, rowNextReview row, rowInterval row,
     rowEaseFactor row, rowRepetitions row, uuid, rowUserId row)

  when (rowsAffected == 0) $ do
    void $ createCardDb conn card

  return card

deleteCardDb :: PG.Connection -> UUID -> UUID -> IO ()
deleteCardDb conn uuid userId = do
  void $ PG.execute conn
    "DELETE FROM flashcards WHERE id = ? AND user_id = ?" [uuid, userId]

getReviewCardsDb :: PG.Connection -> UUID -> IO [API.Flashcard]
getReviewCardsDb conn userId = do
  now <- getCurrentTime
  rows <- PG.query conn
    "SELECT id, front, back, next_review, interval, ease_factor, repetitions, user_id \
    \FROM flashcards \
    \WHERE next_review <= ? AND user_id = ? \
    \ORDER BY next_review ASC" (now, userId)
  return $ map fromRow rows

processReviewDb :: PG.Connection -> UUID -> UUID -> API.ReviewResult -> IO ()
processReviewDb conn uuid userId result = do
  maybeCard <- getCardByIdDb conn uuid userId
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

getDueCountDb :: PG.Connection -> UUID -> IO Int
getDueCountDb conn userId = do
  now <- getCurrentTime
  [PG.Only count] <- PG.query conn
    "SELECT COUNT(*) FROM flashcards WHERE next_review <= ? AND user_id = ?" (now, userId)
  return count

authenticateUserDb :: PG.Connection -> Text -> Text -> IO (Maybe API.User)
authenticateUserDb conn username password = do
  rows <- PG.query conn
    "SELECT userid, username, password, email FROM users \
    \WHERE username = ?" [username]
  case rows of
    [userRow@UserRow{..}] -> do

      let storedHash = TE.encodeUtf8 userPassword
          providedPass = TE.encodeUtf8 password
      if validatePassword storedHash providedPass
        then return $ Just (fromUserRow userRow)
        else return Nothing
    _ -> return Nothing

signupUserDb :: PG.Connection -> API.User -> IO API.User
signupUserDb conn user = do

  let rawPassword = TE.encodeUtf8 $ API.password user
  mHashedPass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy rawPassword
  case mHashedPass of
    Nothing -> error "Failed to hash password"
    Just hashedPass -> do

      let hashedUser = user { API.password = TE.decodeUtf8 hashedPass }
          UserRow{..} = toUserRow hashedUser
      void $ PG.execute conn
        "INSERT INTO users (userid, username, password, email) VALUES (?, ?, ?, ?)"
        (userId, userName, userPassword, userEmail)
      return hashedUser
