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

import API.Types
import API.Client
import API.UUID (SerializableUUID) as UUID
import API.UUID (wrap, unwrap) as UUID
import API.DateTime (SerializableDateTime) as DateTime 
import API.DateTime (wrap, unwrap) as DateTime
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
