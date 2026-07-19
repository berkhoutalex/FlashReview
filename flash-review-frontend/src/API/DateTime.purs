module API.DateTime where

import Prelude

import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))


newtype SerializableDateTime = SerializableDateTime DateTime

-- | The wire format we send to the backend: "YYYY-MM-DDTHH:mm:ss.SSSZ".
-- |
-- | Built as a command list rather than parsed from a pattern string so that
-- | encoding is total. `Formatter.formatDateTime` returns an `Either` only
-- | because it parses its pattern at runtime; `Formatter.format` takes an
-- | already-parsed formatter and cannot fail.
iso8601Formatter :: Formatter.Formatter
iso8601Formatter = List.fromFoldable
  [ Formatter.YearFull
  , Formatter.Placeholder "-"
  , Formatter.MonthTwoDigits
  , Formatter.Placeholder "-"
  , Formatter.DayOfMonthTwoDigits
  , Formatter.Placeholder "T"
  , Formatter.Hours24
  , Formatter.Placeholder ":"
  , Formatter.MinutesTwoDigits
  , Formatter.Placeholder ":"
  , Formatter.SecondsTwoDigits
  , Formatter.Placeholder "."
  , Formatter.Milliseconds
  , Formatter.Placeholder "Z"
  ]

instance encodeJsonSerializableDateTime :: EncodeJson SerializableDateTime where
  encodeJson (SerializableDateTime dt) =
    fromString (Formatter.format iso8601Formatter dt)

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
