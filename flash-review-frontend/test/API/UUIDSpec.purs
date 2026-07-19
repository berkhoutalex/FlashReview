module Test.API.UUIDSpec (spec) where

import Prelude

import API.UUID (SerializableUUID, unwrap, wrap)
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support (parseJson)

sampleText :: String
sampleText = "550e8400-e29b-41d4-a716-446655440000"

decode :: String -> Either JsonDecodeError UUID
decode literal = map unwrap (decodeJson (parseJson literal) :: Either JsonDecodeError SerializableUUID)

spec :: Spec Unit
spec = describe "API.UUID" do

  it "decodes a well-formed UUID string" do
    decode ("\"" <> sampleText <> "\"")
      `shouldEqual` Right (unsafeParse sampleText)

  it "rejects JSON that is not a string" do
    decode "42"
      `shouldEqual` Left (TypeMismatch "Expected a JSON string for UUID")

  it "rejects a string that is not a UUID, echoing the input" do
    decode "\"nope\""
      `shouldEqual` Left (TypeMismatch "Invalid UUID format: nope")

  it "rejects a UUID with a malformed segment" do
    -- One character short in the final group.
    decode "\"550e8400-e29b-41d4-a716-44665544000\""
      `shouldEqual` Left (TypeMismatch "Invalid UUID format: 550e8400-e29b-41d4-a716-44665544000")

  it "encodes to the plain hyphenated string" do
    toString (encodeJson (wrap (unsafeParse sampleText)))
      `shouldEqual` Just sampleText

  it "round-trips" do
    let original = unsafeParse sampleText
    map unwrap (decodeJson (encodeJson (wrap original)) :: Either JsonDecodeError SerializableUUID)
      `shouldEqual` Right original

-- | Parse a UUID literal written inline in a test; a bad literal is a broken
-- | test rather than a case under test.
unsafeParse :: String -> UUID
unsafeParse str = case UUID.parseUUID str of
  Just uuid -> uuid
  Nothing -> unsafeCrashWith ("Test wrote a malformed UUID: " <> str)
