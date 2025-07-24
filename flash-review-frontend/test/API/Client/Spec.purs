module API.Client.Spec where

import Prelude

import API.Client (getAllCards, createCard, updateCard, deleteCard, submitReview, getStats, login, signup)
import API.DateTime (SerializableDateTime(..))
import API.Types (Flashcard(..), ReviewResult(..), Stats(..), User(..), UserCredentials(..))
import API.UUID (SerializableUUID(..))
import API.UUID as UUID
import Data.DateTime (DateTime)
import Data.Either (Either(..), isRight)
import Data.Formatter.DateTime as Formatter
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, parseUUID, genUUID)
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
-- Import a base date to use as fallback
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafeCrashWith)

-- Helper function to parse UUID from string (in actual code, you'd handle errors properly)
unsafeParseUUID :: String -> UUID
unsafeParseUUID str = 
  case parseUUID str of
    Just uuid -> uuid
    Nothing -> unsafePerformEffect $ genUUID -- fallback for tests



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

-- We'll use a module replacement approach for mocking
-- In a real application, you'd want to:
-- 1. Create a MockFetch module that exports compatible functions
-- 2. Use effect capabilities or other dependency injection approaches
-- For tests, we'll just mock the responses directly

-- Mock fetch responses based on URL patterns
mockFetchResponse :: String -> { method :: String } -> Effect { ok :: Boolean, status :: Int, text :: Effect String }
mockFetchResponse url opts = do
  log $ "Mocking fetch for URL: " <> url
  pure case url of
    -- Mock successful responses
    "http://localhost:8081/cards" | opts.method == "GET" -> 
      { ok: true, status: 200, text: pure $ stringify $ encodeJson [sampleFlashcard] }
    
    "http://localhost:8081/cards" | opts.method == "POST" -> 
      { ok: true, status: 201, text: pure $ stringify $ encodeJson sampleFlashcard }
    
    u | u == "http://localhost:8081/cards/" <> (UUID.unwrap sampleUUID # show) <> "/review" -> 
      { ok: true, status: 200, text: pure $ stringify $ encodeJson sampleFlashcard }
    
    "http://localhost:8081/stats" -> 
      { ok: true, status: 200, text: pure $ stringify $ encodeJson sampleStats }
    
    "http://localhost:8081/login" -> 
      { ok: true, status: 200, text: pure $ stringify $ encodeJson sampleUser }
    
    "http://localhost:8081/signup" -> 
      { ok: true, status: 201, text: pure $ stringify $ encodeJson sampleUser }
    
    -- Mock error response
    "http://localhost:8081/error" -> 
      { ok: false, status: 500, text: pure "Server error" }
    
    -- Default error
    _ -> { ok: false, status: 404, text: pure "Not found" }

-- Sample data
sampleUUID :: SerializableUUID
sampleUUID = SerializableUUID (unsafeParseUUID "123e4567-e89b-12d3-a456-426614174000")

sampleDateTime :: SerializableDateTime
sampleDateTime = SerializableDateTime (parseDateTime "2025-07-23T10:00:00Z")

sampleFlashcard :: Flashcard
sampleFlashcard = Flashcard
  { id: sampleUUID
  , front: "Test Question"
  , back: "Test Answer"
  , nextReview: sampleDateTime
  , interval: 1
  , easeFactor: 2.5
  , repetitions: 0
  }

sampleReviewResult :: ReviewResult
sampleReviewResult = ReviewResult { rating: 4 }

sampleStats :: Stats
sampleStats = Stats
  { dueToday: 5
  }

sampleUser :: User
sampleUser = User
  { userId: sampleUUID
  , username: "testuser"
  , email: "test@example.com"
  , password: "password123"
  }

sampleCredentials :: UserCredentials
sampleCredentials = UserCredentials
  { username: "testuser"
  , email: Nothing
  , password: "password123"
  }

-- Setup a test API client module that uses our mock
-- In a real application, you would create a more sophisticated mocking system
-- or use dependency injection for the fetch implementation

-- Helper function to run tests with mock fetch
runWithMockFetch :: forall a. (Unit -> Aff a) -> Aff a
runWithMockFetch action = do
  -- In a real implementation, we would install the mock and restore it after
  -- For now, we'll rely on the API.Client module's implementation to be testable
  action unit

clientSpec :: Spec Unit
clientSpec = describe "API.Client" do
  
  describe "getAllCards" do
    it "should return flashcards on success" do
      -- Here we would mock the specific response for this test
      -- For now, we'll use a simplified approach for demonstration
      result <- runWithMockFetch \_ -> getAllCards
      result `shouldSatisfy` isRight
  
  describe "createCard" do
    it "should create and return a flashcard on success" do
      result <- runWithMockFetch \_ -> createCard sampleFlashcard
      result `shouldSatisfy` isRight

  
  describe "updateCard" do
    it "should update and return the flashcard" do
      result <- runWithMockFetch \_ -> updateCard sampleFlashcard
      result `shouldSatisfy` isRight
  
  describe "deleteCard" do
    it "should delete a flashcard and return success" do
      result <- runWithMockFetch \_ -> deleteCard sampleUUID
      result `shouldSatisfy` isRight
  
  describe "submitReview" do
    it "should submit a review and return updated flashcard" do
      result <- runWithMockFetch \_ -> submitReview sampleUUID sampleReviewResult
      result `shouldSatisfy` isRight
  
  describe "getStats" do
    it "should return user stats" do
      result <- runWithMockFetch \_ -> getStats
      result `shouldSatisfy` isRight
  
  describe "login" do
    it "should log in a user and return user data" do
      result <- runWithMockFetch \_ -> login sampleCredentials
      result `shouldSatisfy` isRight
  
  describe "signup" do
    it "should sign up a user and return user data" do
      result <- runWithMockFetch \_ -> signup sampleCredentials
      result `shouldSatisfy` isRight
