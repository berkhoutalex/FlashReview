module API 
  ( module API.Types
  , module API.Client
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

type SerializableUUID = UUID.SerializableUUID
type SerializableDateTime = DateTime.SerializableDateTime

unwrapUUID :: UUID.SerializableUUID -> UUID
unwrapUUID = UUID.unwrap

wrapUUID :: UUID -> UUID.SerializableUUID
wrapUUID = UUID.wrap

unwrapDateTime :: DateTime.SerializableDateTime -> DateTime
unwrapDateTime = DateTime.unwrap

wrapDateTime :: DateTime -> DateTime.SerializableDateTime
wrapDateTime = DateTime.wrap
