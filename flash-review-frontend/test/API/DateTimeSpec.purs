-- | Tests for the DateTime wire format shared with the Haskell backend.
-- |
-- | The backend encodes `nextReview :: UTCTime` with aeson's default instance,
-- | which does not emit a single stable shape: subsecond precision is omitted
-- | when zero and carried at up to picosecond width when not. The decoder's
-- | fallback chain exists to absorb that variation, so each branch gets a test
-- | using a string aeson can actually produce.
module Test.API.DateTimeSpec (spec) where

import Prelude

import API.DateTime (SerializableDateTime, unwrap, wrap)
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support (mkDateTime, parseJson)

-- | Decode a JSON string literal to a plain `DateTime`, discarding the
-- | newtype so we can compare with `Eq`/`Show`.
decode :: String -> Either JsonDecodeError DateTime
decode literal = map unwrap (decodeJson (parseJson literal) :: Either JsonDecodeError SerializableDateTime)

spec :: Spec Unit
spec = describe "API.DateTime" do

  describe "decoding the aeson shapes the backend emits" do

    it "accepts millisecond precision (YYYY-MM-DDTHH:mm:ss.SSSZ)" do
      decode "\"2024-01-15T10:30:00.123Z\""
        `shouldEqual` Right (mkDateTime 2024 1 15 10 30 0 123)

    it "accepts a timestamp with no fractional part, as aeson emits when subseconds are zero" do
      decode "\"2024-01-15T10:30:00Z\""
        `shouldEqual` Right (mkDateTime 2024 1 15 10 30 0 0)

    it "accepts the space-separated form (YYYY-MM-DD HH:mm:ss)" do
      decode "\"2024-01-15 10:30:00\""
        `shouldEqual` Right (mkDateTime 2024 1 15 10 30 0 0)

    it "accepts sub-millisecond precision by DISCARDING the fraction entirely" do
      -- aeson emits up to 12 fractional digits at full picosecond precision.
      -- The final fallback truncates at the "." rather than rounding, so the
      -- milliseconds land at 0 and not 123. This is lossy on purpose: the app
      -- schedules reviews days out, so sub-second drift is irrelevant.
      decode "\"2024-01-15T10:30:00.123456789012Z\""
        `shouldEqual` Right (mkDateTime 2024 1 15 10 30 0 0)

  describe "rejecting input it cannot understand" do

    it "rejects JSON that is not a string" do
      decode "42"
        `shouldEqual` Left (TypeMismatch "Expected a JSON string for DateTime")

    it "rejects a string that is not a timestamp, echoing the input" do
      decode "\"not a date\""
        `shouldEqual` Left (TypeMismatch "Invalid DateTime format: not a date")

  describe "encoding" do

    it "encodes to the millisecond-precision form the backend parses" do
      toString (encodeJson (wrap (mkDateTime 2024 1 15 10 30 0 123)))
        `shouldEqual` Just "2024-01-15T10:30:00.123Z"

    it "pads single-digit components to fixed width" do
      toString (encodeJson (wrap (mkDateTime 2024 3 5 9 7 6 5)))
        `shouldEqual` Just "2024-03-05T09:07:06.005Z"

    it "round-trips at millisecond precision" do
      let original = mkDateTime 2024 1 15 10 30 0 123
      map unwrap (decodeJson (encodeJson (wrap original)) :: Either JsonDecodeError SerializableDateTime)
        `shouldEqual` Right original
