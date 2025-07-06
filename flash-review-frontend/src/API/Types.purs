module API.Types where

import Prelude

import API.DateTime (SerializableDateTime)
import API.UUID (SerializableUUID)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

-- | Flashcard data type that mirrors the Haskell API
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
  decodeJson = genericDecodeJson

-- | Review result data type
newtype ReviewResult = ReviewResult
  { rating :: Int -- e.g., 0..5
  }

derive instance genericReviewResult :: Generic ReviewResult _
instance encodeJsonReviewResult :: EncodeJson ReviewResult where
  encodeJson (ReviewResult record) = encodeJson record
instance decodeJsonReviewResult :: DecodeJson ReviewResult where
  decodeJson = genericDecodeJson

-- | Statistics data type
newtype Stats = Stats
  { dueToday :: Int
  }

derive instance genericStats :: Generic Stats _
instance encodeJsonStats :: EncodeJson Stats where
  encodeJson (Stats record) = encodeJson record
instance decodeJsonStats :: DecodeJson Stats where
  decodeJson = genericDecodeJson
