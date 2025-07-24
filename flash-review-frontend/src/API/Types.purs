module API.Types where

import Prelude

import API.DateTime (SerializableDateTime)
import API.DateTime as DateTime
import API.UUID (SerializableUUID)
import API.UUID as UUID
import Data.Argonaut.Decode (class DecodeJson, (.:), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

newtype Flashcard = Flashcard
  { id :: SerializableUUID
  , front :: String
  , back :: String
  , nextReview :: SerializableDateTime
  , interval :: Int
  , easeFactor :: Number
  , repetitions :: Int
  }
derive instance Eq Flashcard
instance Show Flashcard where
  show (Flashcard { id, front, back, nextReview, interval, easeFactor, repetitions }) =
    "Flashcard { id: " <> show (UUID.unwrap id)
    <> ", front: " <> front
    <> ", back: " <> back
    <> ", nextReview: " <> show (DateTime.unwrap nextReview)
    <> ", interval: " <> show interval
    <> ", easeFactor: " <> show easeFactor
    <> ", repetitions: " <> show repetitions
    <> " }"

derive instance genericFlashcard :: Generic Flashcard _
instance encodeJsonFlashcard :: EncodeJson Flashcard where
  encodeJson (Flashcard record) = encodeJson record
instance decodeJsonFlashcard :: DecodeJson Flashcard where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    front <- obj .: "front"
    back <- obj .: "back"
    nextReview <- obj .: "nextReview"
    interval <- obj .: "interval"
    easeFactor <- obj .: "easeFactor"
    repetitions <- obj .: "repetitions"
    pure $ Flashcard
      { id
      , front
      , back
      , nextReview
      , interval
      , easeFactor
      , repetitions
      }

newtype ReviewResult = ReviewResult
  { rating :: Int
  }
derive instance Eq ReviewResult
instance Show ReviewResult where
  show (ReviewResult { rating }) = "ReviewResult { rating: " <> show rating <> " }"
derive instance genericReviewResult :: Generic ReviewResult _
instance encodeJsonReviewResult :: EncodeJson ReviewResult where
  encodeJson (ReviewResult record) = encodeJson record
instance decodeJsonReviewResult :: DecodeJson ReviewResult where
  decodeJson json = do
    obj <- decodeJson json
    rating <- obj .: "rating"
    pure $ ReviewResult { rating }

newtype Stats = Stats
  { dueToday :: Int
  }
instance Show Stats where
  show (Stats { dueToday }) = "Stats { dueToday: " <> show dueToday <> " }"
derive instance genericStats :: Generic Stats _
instance encodeJsonStats :: EncodeJson Stats where
  encodeJson (Stats record) = encodeJson record
instance decodeJsonStats :: DecodeJson Stats where
  decodeJson json = do
    obj <- decodeJson json
    dueToday <- obj .: "dueToday"
    pure $ Stats { dueToday }

newtype User = User
  { userId :: SerializableUUID
  , username :: String
  , email :: String
  , password :: String
  }
derive instance Eq User
instance Show User where
  show (User { userId, username, email }) =
    "User { userId: " <> show (UUID.unwrap userId)
    <> ", username: " <> username
    <> ", email: " <> email
    <> ", password: " <> "redacted"
    <> " }"
derive instance genericUser :: Generic User _
instance encodeJsonUser :: EncodeJson User where
  encodeJson (User record) = encodeJson record
instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    userId <- obj .: "userId"
    username <- obj .: "username"
    email <- obj .: "email"
    password <- obj .: "password"
    pure $ User { userId, username, email, password }


newtype UserCredentials = UserCredentials
  { username :: String
  , email :: Maybe String
  , password :: String
  }
instance Show UserCredentials where
  show (UserCredentials { username, email }) =
    "UserCredentials { username: " <> username
    <> ", email: " <> show email
    <> ", password: " <> "redacted"
    <> " }"
derive instance Eq UserCredentials
derive instance genericUserCredentials :: Generic UserCredentials _
instance encodeJsonUserCredentials :: EncodeJson UserCredentials where
  encodeJson (UserCredentials record) = encodeJson record
instance decodeJsonUserCredentials :: DecodeJson UserCredentials where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    email <- obj .: "email"
    password <- obj .: "password"
    pure $ UserCredentials { username, email, password }
