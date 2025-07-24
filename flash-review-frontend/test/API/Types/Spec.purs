module API.Types.Spec where

import Prelude

import API.DateTime (SerializableDateTime(..))
import API.Types (Flashcard(..), ReviewResult(..), User(..), UserCredentials(..))
import API.UUID (SerializableUUID(..))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either(..), isRight)
import Data.Formatter.DateTime as Formatter
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
-- Import a base date to use as fallback
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))

-- Helper function to parse UUID from string (in actual code, you'd handle errors properly)
unsafeParseUUID :: String -> UUID
unsafeParseUUID str = 
  case parseUUID str of
    Just uuid -> uuid
    Nothing -> unsafePerformEffect $ UUID.genUUID -- fallback for tests



-- Default datetime for testing
defaultDateTime :: DateTime
defaultDateTime = 
  case instant (Milliseconds 1627023600000.0) of -- July 23, 2025
    Just i -> toDateTime i
    Nothing -> unsafePerformEffect do
      let parseResult = Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" "2025-07-23T10:00:00.000Z"
      case parseResult of
        Right dt -> pure dt
        Left _ -> do
          let maybeInstant = instant (Milliseconds 0.0)
          case maybeInstant of
            Just i -> pure (toDateTime i)
            Nothing -> unsafeCrashWith "Could not create instant"

-- Helper function to parse DateTime from string
parseDateTime :: String -> DateTime
parseDateTime str = 
  case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" str of
    Right dt -> dt
    Left _ -> case Formatter.unformatDateTime "YYYY-MM-DDTHH:mm:ss.SSSZ" "2025-07-23T10:00:00.000Z" of
      Right dt -> dt
      Left _ -> defaultDateTime

typesSpec :: Spec Unit
typesSpec = describe "API.Types" do
  
  describe "Flashcard" do
    let 
      testId = SerializableUUID (unsafeParseUUID "123e4567-e89b-12d3-a456-426614174000")
      testDate = SerializableDateTime (parseDateTime "2025-07-23T10:00:00Z")
      flashcard = Flashcard 
        { id: testId
        , front: "Question"
        , back: "Answer"
        , nextReview: testDate
        , interval: 1
        , easeFactor: 2.5
        , repetitions: 0
        }
    
    it "should encode and decode correctly" do
      let 
        encoded = encodeJson flashcard
        decoded = decodeJson encoded :: Either _ Flashcard
      
      decoded `shouldSatisfy` isRight
      decoded `shouldEqual` Right flashcard
  
  describe "ReviewResult" do
    let 
      reviewResult = ReviewResult { rating: 4 }
    
    it "should encode and decode correctly" do
      let 
        encoded = encodeJson reviewResult
        decoded = decodeJson encoded :: Either _ ReviewResult
      
      decoded `shouldSatisfy` isRight
      decoded `shouldEqual` Right reviewResult
  
  describe "User" do
    let 
      user = User 
        { userId: SerializableUUID (unsafeParseUUID "123e4567-e89b-12d3-a456-426614174000")
        , username: "testuser"
        , email: "test@example.com"
        , password: "password123"
        }
    
    it "should encode and decode correctly" do
      let 
        encoded = encodeJson user
        decoded = decodeJson encoded :: Either _ User
      
      decoded `shouldSatisfy` isRight
      decoded `shouldEqual` Right user
  
  describe "UserCredentials" do
    let 
      credentials = UserCredentials 
        { username: "testuser"
        , password: "password123"
        , email: Nothing
        }
    
    it "should encode and decode correctly" do
      let 
        encoded = encodeJson credentials
        decoded = decodeJson encoded :: Either _ UserCredentials
      
      decoded `shouldSatisfy` isRight
      decoded `shouldEqual` Right credentials
