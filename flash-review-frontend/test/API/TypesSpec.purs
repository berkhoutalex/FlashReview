-- | Tests for the request/response payloads exchanged with the backend.
-- |
-- | The JSON literals here mirror what `flash-review-backend/src/API.hs`
-- | actually emits: those types derive their aeson instances generically with
-- | no field-label modifier, so the wire names match the Haskell record fields
-- | verbatim. If someone renames a field on either side, these fail.
module Test.API.TypesSpec (spec) where

import Prelude

import API.DateTime (unwrap) as DateTime
import API.Types (Flashcard(..), ReviewResult(..), Stats(..), User(..))
import API.UUID (unwrap) as UUID
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), isLeft)
import Data.UUID (toString) as UUID
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Support (mkDateTime, parseJson)

flashcardJson :: String
flashcardJson =
  """
  { "id": "550e8400-e29b-41d4-a716-446655440000"
  , "front": "What is a monad?"
  , "back": "A monoid in the category of endofunctors"
  , "nextReview": "2024-01-15T10:30:00Z"
  , "interval": 1
  , "easeFactor": 2.5
  , "repetitions": 0
  }
  """

decodeFlashcard :: String -> Either JsonDecodeError Flashcard
decodeFlashcard literal = decodeJson (parseJson literal)

spec :: Spec Unit
spec = describe "API.Types" do

  describe "Flashcard" do

    it "decodes a card as the backend serialises it" do
      case decodeFlashcard flashcardJson of
        Left err -> fail ("expected a Flashcard, got " <> show err)
        Right (Flashcard card) -> do
          UUID.toString (UUID.unwrap card.id) `shouldEqual` "550e8400-e29b-41d4-a716-446655440000"
          card.front `shouldEqual` "What is a monad?"
          card.back `shouldEqual` "A monoid in the category of endofunctors"
          DateTime.unwrap card.nextReview `shouldEqual` mkDateTime 2024 1 15 10 30 0 0
          card.interval `shouldEqual` 1
          card.easeFactor `shouldEqual` 2.5
          card.repetitions `shouldEqual` 0

    it "round-trips through encode and decode" do
      case decodeFlashcard flashcardJson of
        Left err -> fail ("expected a Flashcard, got " <> show err)
        Right card@(Flashcard original) ->
          case (decodeJson (encodeJson card) :: Either JsonDecodeError Flashcard) of
            Left err -> fail ("re-decode failed: " <> show err)
            Right (Flashcard roundTripped) -> do
              roundTripped.front `shouldEqual` original.front
              roundTripped.back `shouldEqual` original.back
              roundTripped.interval `shouldEqual` original.interval
              roundTripped.easeFactor `shouldEqual` original.easeFactor
              roundTripped.repetitions `shouldEqual` original.repetitions
              DateTime.unwrap roundTripped.nextReview
                `shouldEqual` DateTime.unwrap original.nextReview
              UUID.unwrap roundTripped.id `shouldEqual` UUID.unwrap original.id

    it "fails when a required field is absent" do
      -- "repetitions" omitted.
      let missing =
            """
            { "id": "550e8400-e29b-41d4-a716-446655440000"
            , "front": "front", "back": "back"
            , "nextReview": "2024-01-15T10:30:00Z"
            , "interval": 1, "easeFactor": 2.5
            }
            """
      isLeft (decodeFlashcard missing) `shouldEqual` true

    it "fails when a field has the wrong type" do
      -- "interval" as a string rather than a number.
      let wrongType =
            """
            { "id": "550e8400-e29b-41d4-a716-446655440000"
            , "front": "front", "back": "back"
            , "nextReview": "2024-01-15T10:30:00Z"
            , "interval": "1", "easeFactor": 2.5, "repetitions": 0
            }
            """
      isLeft (decodeFlashcard wrongType) `shouldEqual` true

  describe "Stats" do

    it "decodes the due count" do
      case (decodeJson (parseJson """{ "dueToday": 7 }""") :: Either JsonDecodeError Stats) of
        Left err -> fail ("expected Stats, got " <> show err)
        Right (Stats stats) -> stats.dueToday `shouldEqual` 7

  describe "ReviewResult" do

    it "decodes a rating" do
      case (decodeJson (parseJson """{ "rating": 4 }""") :: Either JsonDecodeError ReviewResult) of
        Left err -> fail ("expected a ReviewResult, got " <> show err)
        Right (ReviewResult result) -> result.rating `shouldEqual` 4

    it "round-trips" do
      let encoded = encodeJson (ReviewResult { rating: 3 })
      case (decodeJson encoded :: Either JsonDecodeError ReviewResult) of
        Left err -> fail ("re-decode failed: " <> show err)
        Right (ReviewResult result) -> result.rating `shouldEqual` 3

  describe "User" do

    it "decodes a user record" do
      let json =
            """
            { "userId": "550e8400-e29b-41d4-a716-446655440000"
            , "username": "alex"
            , "email": "alex@example.com"
            , "password": "hunter2"
            }
            """
      case (decodeJson (parseJson json) :: Either JsonDecodeError User) of
        Left err -> fail ("expected a User, got " <> show err)
        Right (User user) -> do
          user.username `shouldEqual` "alex"
          user.email `shouldEqual` "alex@example.com"
          UUID.toString (UUID.unwrap user.userId)
            `shouldEqual` "550e8400-e29b-41d4-a716-446655440000"
