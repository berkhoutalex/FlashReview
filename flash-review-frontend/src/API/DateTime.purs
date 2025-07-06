module API.DateTime where

import Prelude

import Data.Argonaut.Core (Json, fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Formatter.DateTime as Formatter
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Newtype wrapper for DateTime to avoid orphan instances
newtype SerializableDateTime = SerializableDateTime DateTime

-- | Encode a SerializableDateTime to JSON - using a pure formatter
instance encodeJsonSerializableDateTime :: EncodeJson SerializableDateTime where
  encodeJson (SerializableDateTime dt) = 
    case Formatter.formatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" dt of
      Left err -> fromString $ "Invalid DateTime: " <> err  -- This is a fallback that shouldn't happen
      Right formatted -> fromString formatted

-- | Decode a SerializableDateTime from JSON
instance decodeJsonSerializableDateTime :: DecodeJson SerializableDateTime where
  decodeJson json = 
    case toString json of
      Nothing -> Left $ TypeMismatch "Expected a JSON string for DateTime"
      Just str -> 
        case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" str of
          Right dt -> Right (SerializableDateTime dt)
          Left err -> Left $ TypeMismatch $ "Invalid DateTime format: " <> str <> " - " <> err

-- | Unwrap the DateTime from SerializableDateTime
unwrap :: SerializableDateTime -> DateTime
unwrap (SerializableDateTime dt) = dt

-- | Wrap a DateTime in SerializableDateTime
wrap :: DateTime -> SerializableDateTime
wrap = SerializableDateTime
