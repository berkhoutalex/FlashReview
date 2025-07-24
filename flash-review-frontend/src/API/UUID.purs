module API.UUID where

import Prelude

import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID

newtype SerializableUUID = SerializableUUID UUID
derive instance Eq SerializableUUID
instance encodeJsonSerializableUUID :: EncodeJson SerializableUUID where
  encodeJson (SerializableUUID uuid) = fromString (UUID.toString uuid)

instance decodeJsonSerializableUUID :: DecodeJson SerializableUUID where
  decodeJson json = 
    case toString json of
      Nothing -> Left $ TypeMismatch "Expected a JSON string for UUID"
      Just str -> 
        case UUID.parseUUID str of
          Just uuid -> Right (SerializableUUID uuid)
          Nothing -> Left $ TypeMismatch $ "Invalid UUID format: " <> str

unwrap :: SerializableUUID -> UUID
unwrap (SerializableUUID uuid) = uuid

wrap :: UUID -> SerializableUUID
wrap = SerializableUUID
