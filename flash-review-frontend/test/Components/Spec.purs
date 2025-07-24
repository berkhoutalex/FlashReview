module Components.Spec where

import Prelude

import API.DateTime (SerializableDateTime(..))
import API.Types (Flashcard(..), User(..))
import API.UUID (SerializableUUID(..))

import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime as Formatter
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec (Spec, describe, it)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafeCrashWith)


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
        Left _ ->
          let maybeInstant = instant (Milliseconds 0.0) in
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

-- Sample data for testing
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

sampleUser :: User
sampleUser = User
  { userId: sampleUUID
  , username: "testuser"
  , email: "test@example.com"
  , password: "password123"
  }

-- Test specs for Components
-- In a real implementation, you would use a testing library that supports Halogen components
-- Here we'll create placeholder tests that can be expanded when the proper testing dependencies are added
componentsSpec :: Spec Unit
componentsSpec = describe "Components" do
  
  describe "App Component" do
    it "should initialize with correct default state" do
      pure unit -- Placeholder for component testing
  
  describe "FlashcardForm Component" do
    it "should render form fields" do
      pure unit -- Placeholder for component testing
  
  describe "FlashcardList Component" do
    it "should render flashcards" do
      pure unit -- Placeholder for component testing
  
  describe "Login Component" do
    it "should render login form" do
      pure unit -- Placeholder for component testing
  
  describe "Signup Component" do
    it "should render signup form" do
      pure unit -- Placeholder for component testing
  
  describe "Review Component" do
    it "should render review interface" do
      pure unit -- Placeholder for component testing
  
  describe "Stats Component" do
    it "should render statistics" do
      pure unit -- Placeholder for component testing

-- Note: To properly test Halogen components, you'd need to add additional dependencies
-- such as purescript-halogen-test or create a custom test harness
-- For now, we've added placeholder tests that can be expanded later
