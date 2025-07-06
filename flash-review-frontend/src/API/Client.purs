module API.Client where

import Prelude

import API.Types (Flashcard(..), ReviewResult(..), Stats(..))
import API.UUID (SerializableUUID)
import API.UUID as UUID
import Data.Argonaut.Decode (decodeJson, JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (Json, stringify)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Effect.Class (liftEffect)
import Fetch (fetch, Method(..), Response)

-- Base API URL
baseUrl :: String
baseUrl = "http://localhost:8081"

-- | Helper function to convert JsonDecodeError to String
handleDecodeResult :: forall a. Either JsonDecodeError a -> Either String a
handleDecodeResult = either (Left <<< printJsonDecodeError) Right

-- | Helper function to handle fetch responses
handleJsonResponse :: forall a. (Json -> Either JsonDecodeError a) -> Response -> Aff (Either String a)
handleJsonResponse decoder response = do
  if response.ok
    then do
      text <- response.text
      case jsonParser text of
        Left err -> pure $ Left $ "Failed to parse JSON: " <> err
        Right json -> pure $ handleDecodeResult $ decoder json
    else
      pure $ Left $ "Request failed with status: " <> show response.status

-- | Get all flashcards
getAllCards :: Aff (Either String (Array Flashcard))
getAllCards = do
  response <- fetch (baseUrl <> "/cards") {}
  handleJsonResponse decodeJson response

-- | Create a new flashcard
createCard :: Flashcard -> Aff (Either String Flashcard)
createCard card = do
  let opts = 
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: stringify (encodeJson card)
        }
  response <- fetch (baseUrl <> "/cards") opts
  handleJsonResponse decodeJson response

-- | Update an existing flashcard
updateCard :: Flashcard -> Aff (Either String Flashcard)
updateCard card@(Flashcard c) = do
  let idString = UUID.toString (UUID.unwrap c.id)
      opts = 
        { method: PUT
        , headers: { "Content-Type": "application/json" }
        , body: stringify (encodeJson card)
        }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  handleJsonResponse decodeJson response

-- | Delete a flashcard
deleteCard :: SerializableUUID -> Aff (Either String Unit)
deleteCard id = do
  let idString = UUID.toString (UUID.unwrap id)
      opts = { method: DELETE }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "DELETE /cards/" <> idString <> " request failed with status: " <> show response.status

-- | Get cards due for review
getReviewQueue :: Aff (Either String (Array Flashcard))
getReviewQueue = do
  response <- fetch (baseUrl <> "/review/queue") {}
  handleJsonResponse decodeJson response

-- | Submit a review result for a card
submitReview :: SerializableUUID -> ReviewResult -> Aff (Either String Unit)
submitReview id result = do
  let idString = UUID.toString (UUID.unwrap id)
      opts = 
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: stringify (encodeJson result)
        }
  response <- fetch (baseUrl <> "/review/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "POST /review/" <> idString <> " request failed with status: " <> show response.status

-- | Get statistics
getStats :: Aff (Either String Stats)
getStats = do
  response <- fetch (baseUrl <> "/stats") {}
  handleJsonResponse decodeJson response
