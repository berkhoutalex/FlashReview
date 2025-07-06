module API.DateTime where

import Prelude

import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime as Formatter
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))


newtype SerializableDateTime = SerializableDateTime DateTime

instance encodeJsonSerializableDateTime :: EncodeJson SerializableDateTime where
  encodeJson (SerializableDateTime dt) = 
    case Formatter.formatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" dt of
      Left err -> fromString $ "Invalid DateTime: " <> err
      Right formatted -> fromString formatted

instance decodeJsonSerializableDateTime :: DecodeJson SerializableDateTime where
  decodeJson json = 
    case toString json of
      Nothing -> Left $ TypeMismatch "Expected a JSON string for DateTime"
      Just str -> 
        -- Try with the primary format first (with milliseconds)
        case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" str of
          Right dt -> Right (SerializableDateTime dt)
          Left _ -> 
            -- Try without milliseconds
            case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" str of
              Right dt -> Right (SerializableDateTime dt)
              Left _ ->
                -- Try a more lenient pattern to handle non-standard formats
                case Formatter.unformatDateTime "YYYY-MM-DD HH:mm:ss" str of
                  Right dt -> Right (SerializableDateTime dt)
                  Left _ ->
                    -- As a last resort, try to manually adjust the format for cases like "2025-07-06T18:36:38.80149Z"
                    -- We know this is the format causing issues from the error message
                    let
                      -- Simple modification to handle ".80149Z" format - truncate to just "Z"
                      modifiedStr = 
                        case String.indexOf (Pattern ".") str of
                          Just idx -> 
                            -- If we found a dot, truncate everything from there to before the Z
                            let basePart = String.take idx str
                            in basePart <> "Z"
                          Nothing -> str
                    in
                      case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" modifiedStr of
                        Right dt -> Right (SerializableDateTime dt)
                        Left _ -> Left $ TypeMismatch $ "Invalid DateTime format: " <> str

unwrap :: SerializableDateTime -> DateTime
unwrap (SerializableDateTime dt) = dt

wrap :: DateTime -> SerializableDateTime
wrap = SerializableDateTime
