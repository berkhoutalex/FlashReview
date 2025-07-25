module API.Client where

import Prelude

import API.Types (Flashcard(..), ReviewResult, Stats, User, UserCredentials)
import API.UUID (SerializableUUID)
import API.UUID (unwrap) as UUID
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.UUID (toString) as UUID
import Effect.Aff (Aff)
import Fetch (fetch, Method(..), Response, RequestCredentials(..))

baseUrl :: String
baseUrl = "http://localhost:8081"

handleDecodeResult :: forall a. Either JsonDecodeError a -> Either String a
handleDecodeResult = either (Left <<< printJsonDecodeError) Right

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


getAllCards :: Aff (Either String (Array Flashcard))
getAllCards = do
  response <- fetch (baseUrl <> "/cards") {credentials: Include}
  handleJsonResponse decodeJson response

createCard :: Flashcard -> Aff (Either String Flashcard)
createCard card = do
  let opts = 
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString card
        , credentials: Include
        }
  response <- fetch (baseUrl <> "/cards") opts
  handleJsonResponse decodeJson response

updateCard :: Flashcard -> Aff (Either String Flashcard)
updateCard card@(Flashcard c) = do
  let idString = UUID.toString (UUID.unwrap c.id)
      opts = 
        { method: PUT
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString card
        , credentials: Include
        }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  handleJsonResponse decodeJson response

deleteCard :: SerializableUUID -> Aff (Either String Unit)
deleteCard id = do
  let idString = UUID.toString (UUID.unwrap id)
      opts = { method: DELETE, credentials: Include }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "DELETE /cards/" <> idString <> " request failed with status: " <> show response.status

getReviewQueue :: Aff (Either String (Array Flashcard))
getReviewQueue = do
  response <- fetch (baseUrl <> "/review/queue") {credentials: Include}
  handleJsonResponse decodeJson response

submitReview :: SerializableUUID -> ReviewResult -> Aff (Either String Unit)
submitReview id result = do
  let idString = UUID.toString (UUID.unwrap id)
      opts = 
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString result
        , credentials: Include
        }
  response <- fetch (baseUrl <> "/review/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "POST /review/" <> idString <> " request failed with status: " <> show response.status

getStats :: Aff (Either String Stats)
getStats = do
  response <- fetch (baseUrl <> "/stats") {credentials: Include}
  handleJsonResponse decodeJson response

login :: UserCredentials -> Aff (Either String String)
login credentials = do
  let opts = 
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString credentials
        , credentials: Include 
        }
  response <- fetch (baseUrl <> "/login") opts
  if response.ok
    then do
      text <- response.text
      pure $ Right text
    else
      pure $ Left $ "Login failed with status: " <> show response.status

signup :: UserCredentials -> Aff (Either String User)
signup credentials = do
  let opts = 
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString credentials
        , credentials: Include 
        }
  response <- fetch (baseUrl <> "/signup") opts
  handleJsonResponse decodeJson response
