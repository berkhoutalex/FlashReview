module API 
  ( module API.Types
  , module API.Client
  -- Re-export specific types and functions from UUID and DateTime modules
  , SerializableUUID
  , SerializableDateTime
  , wrapUUID
  , wrapDateTime
  , unwrapUUID
  , unwrapDateTime
  ) where

import API.Types (Flashcard(..), ReviewResult(..), Stats(..))
import API.Client (baseUrl, createCard, deleteCard, getAllCards, getReviewQueue, getStats, handleDecodeResult, handleJsonResponse, submitReview, updateCard)
import API.UUID (SerializableUUID, wrap, unwrap) as UUID
import API.DateTime (SerializableDateTime, wrap, unwrap) as DateTime 
import Data.UUID (UUID)
import Data.DateTime (DateTime)

-- Re-export the types under consistent names
type SerializableUUID = UUID.SerializableUUID
type SerializableDateTime = DateTime.SerializableDateTime

-- Re-export with specific names to avoid conflicts
unwrapUUID :: UUID.SerializableUUID -> UUID
unwrapUUID = UUID.unwrap

wrapUUID :: UUID -> UUID.SerializableUUID
wrapUUID = UUID.wrap

unwrapDateTime :: DateTime.SerializableDateTime -> DateTime
unwrapDateTime = DateTime.unwrap

wrapDateTime :: DateTime -> DateTime.SerializableDateTime
wrapDateTime = DateTime.wrap
