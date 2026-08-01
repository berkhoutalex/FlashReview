module API.Client where

import Prelude

import API.Storage as Storage
import API.Types (Flashcard(..), ReviewResult, Stats, User, UserCredentials)
import API.UUID (SerializableUUID)
import API.UUID (unwrap) as UUID
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Data.UUID (toString) as UUID
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Fetch (fetch, Method(..), Response)

-- Replaced at build time by Render's build command, which substitutes
-- $API_BASE_URL for this literal before spago compiles the project.
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

-- | The header is always sent, empty when no token is stored. The backend
-- | treats an unparseable token as unauthenticated and answers 401, which is
-- | the behavior we want for a logged-out client.
authHeaders :: Aff { "Authorization" :: String }
authHeaders = do
  mToken <- liftEffect Storage.getToken
  pure { "Authorization": "Bearer " <> fromMaybe "" mToken }

authJsonHeaders :: Aff { "Authorization" :: String, "Content-Type" :: String }
authJsonHeaders = do
  mToken <- liftEffect Storage.getToken
  pure
    { "Authorization": "Bearer " <> fromMaybe "" mToken
    , "Content-Type": "application/json"
    }

getAllCards :: Aff (Either String (Array Flashcard))
getAllCards = do
  headers <- authHeaders
  response <- fetch (baseUrl <> "/cards") { headers }
  handleJsonResponse decodeJson response

createCard :: Flashcard -> Aff (Either String Flashcard)
createCard card = do
  headers <- authJsonHeaders
  let opts =
        { method: POST
        , headers
        , body: toJsonString card
        }
  response <- fetch (baseUrl <> "/cards") opts
  handleJsonResponse decodeJson response

updateCard :: Flashcard -> Aff (Either String Flashcard)
updateCard card@(Flashcard c) = do
  headers <- authJsonHeaders
  let idString = UUID.toString (UUID.unwrap c.id)
      opts =
        { method: PUT
        , headers
        , body: toJsonString card
        }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  handleJsonResponse decodeJson response

deleteCard :: SerializableUUID -> Aff (Either String Unit)
deleteCard id = do
  headers <- authHeaders
  let idString = UUID.toString (UUID.unwrap id)
      opts = { method: DELETE, headers }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "DELETE /cards/" <> idString <> " request failed with status: " <> show response.status

getReviewQueue :: Aff (Either String (Array Flashcard))
getReviewQueue = do
  headers <- authHeaders
  response <- fetch (baseUrl <> "/review/queue") { headers }
  handleJsonResponse decodeJson response

submitReview :: SerializableUUID -> ReviewResult -> Aff (Either String Unit)
submitReview id result = do
  headers <- authJsonHeaders
  let idString = UUID.toString (UUID.unwrap id)
      opts =
        { method: POST
        , headers
        , body: toJsonString result
        }
  response <- fetch (baseUrl <> "/review/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "POST /review/" <> idString <> " request failed with status: " <> show response.status

getStats :: Aff (Either String Stats)
getStats = do
  headers <- authHeaders
  response <- fetch (baseUrl <> "/stats") { headers }
  handleJsonResponse decodeJson response

login :: UserCredentials -> Aff (Either String String)
login credentials = do
  let opts =
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString credentials
        }
  response <- fetch (baseUrl <> "/login") opts
  -- /login is declared `Post '[JSON] String`, so the body arrives JSON-encoded
  -- with surrounding quotes. Decoding strips them; storing the raw text would
  -- produce a bearer token the backend cannot parse.
  result <- handleJsonResponse decodeJson response
  case result of
    Left err -> pure $ Left $ "Login failed: " <> err
    Right token -> do
      liftEffect $ Storage.setToken token
      pure $ Right token

signup :: UserCredentials -> Aff (Either String User)
signup credentials = do
  let opts =
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString credentials
        }
  response <- fetch (baseUrl <> "/signup") opts
  handleJsonResponse decodeJson response
