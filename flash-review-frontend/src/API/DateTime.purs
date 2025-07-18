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
        case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" str of
          Right dt -> Right (SerializableDateTime dt)
          Left _ -> 
            case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" str of
              Right dt -> Right (SerializableDateTime dt)
              Left _ ->
                case Formatter.unformatDateTime "YYYY-MM-DD HH:mm:ss" str of
                  Right dt -> Right (SerializableDateTime dt)
                  Left _ ->
                    let
                      modifiedStr = 
                        case String.indexOf (Pattern ".") str of
                          Just idx -> 
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
