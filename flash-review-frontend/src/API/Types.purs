module API.Types where

import Prelude

import API.DateTime (SerializableDateTime)
import API.UUID (SerializableUUID)
import Data.Argonaut.Decode (class DecodeJson, (.:), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)

newtype Flashcard = Flashcard
  { id :: SerializableUUID
  , front :: String
  , back :: String
  , nextReview :: SerializableDateTime
  , interval :: Int
  , easeFactor :: Number
  , repetitions :: Int
  }

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

derive instance genericStats :: Generic Stats _
instance encodeJsonStats :: EncodeJson Stats where
  encodeJson (Stats record) = encodeJson record
instance decodeJsonStats :: DecodeJson Stats where
  decodeJson json = do
    obj <- decodeJson json
    dueToday <- obj .: "dueToday"
    pure $ Stats { dueToday }
