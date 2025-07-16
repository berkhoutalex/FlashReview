{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module API(FlashcardAPI, Flashcard(..), FlashcardRequest(..), flashcardToRequest, ReviewResult(..), Stats(..), User(..), UserJWT(..), SignupRequest(..), LoginRequest(..)) where

import           Data.Aeson          (FromJSON, ToJSON (toJSON), defaultOptions,
                                      fieldLabelModifier, genericParseJSON,
                                      genericToJSON)
import           Data.Aeson.Types    (FromJSON (parseJSON))
import           Data.Char           (toLower)
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           Data.UUID           (UUID)
import           GHC.Generics        (Generic)
import           Servant
import           Servant.Auth.Server

data Flashcard = Flashcard
  { id          :: UUID
  , front       :: Text
  , back        :: Text
  , nextReview  :: UTCTime
  , interval    :: Int
  , easeFactor  :: Double
  , repetitions :: Int
  , ownerId     :: UUID
  } deriving (Generic, Show)

instance ToJSON Flashcard
instance FromJSON Flashcard

-- FlashcardRequest is used for creating or updating flashcards
-- ownerId is omitted as it's determined from the authenticated user
data FlashcardRequest = FlashcardRequest
  { reqId          :: UUID
  , reqFront       :: Text
  , reqBack        :: Text
  , reqNextReview  :: UTCTime
  , reqInterval    :: Int
  , reqEaseFactor  :: Double
  , reqRepetitions :: Int
  } deriving (Generic, Show)

instance ToJSON FlashcardRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \s -> case drop (length "req") s of
    (x:xs) -> toLower x : xs
    []     -> []
  }
instance FromJSON FlashcardRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \s -> case drop (length "req") s of
    (x:xs) -> toLower x : xs
    []     -> []
  }

flashcardToRequest :: Flashcard -> FlashcardRequest
flashcardToRequest Flashcard{..} = FlashcardRequest
  { reqId          = id
  , reqFront       = front
  , reqBack        = back
  , reqNextReview  = nextReview
  , reqInterval    = interval
  , reqEaseFactor  = easeFactor
  , reqRepetitions = repetitions
  }

type FlashcardAPI auths =
       Auth auths UserJWT :> "cards" :> Get '[JSON] [Flashcard]
  :<|> Auth auths UserJWT :> "cards" :> ReqBody '[JSON] FlashcardRequest :> Post '[JSON] Flashcard
  :<|> Auth auths UserJWT :> "cards" :> Capture "id" UUID :> ReqBody '[JSON] FlashcardRequest :> Put '[JSON] Flashcard
  :<|> Auth auths UserJWT :> "cards" :> Capture "id" UUID :> Delete '[JSON] NoContent
  :<|> Auth auths UserJWT :> "review" :> "queue" :> Get '[JSON] [Flashcard]
  :<|> Auth auths UserJWT :> "review" :> Capture "id" UUID :> ReqBody '[JSON] ReviewResult :> Post '[JSON] NoContent
  :<|> Auth auths UserJWT :> "stats" :> Get '[JSON] Stats
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
  :<|> "signup" :> ReqBody '[JSON] SignupRequest :> Post '[JSON] User

newtype ReviewResult = ReviewResult
  { rating :: Int
  } deriving (Generic)

instance ToJSON ReviewResult
instance FromJSON ReviewResult

newtype Stats = Stats
  { dueToday :: Int
  } deriving (Generic)

instance ToJSON Stats
instance FromJSON Stats

data User = User
  { userId   :: UUID
  , username :: Text
  , email    :: Text
  , password :: Text
  } deriving (Generic)


data UserJWT = UserJWT
  { userJwtId    :: UUID
  , userJwtName  :: Text
  , userJwtEmail :: Text
  } deriving (Generic, FromJWT, ToJWT)

instance ToJSON UserJWT
instance FromJSON UserJWT

instance ToJSON User
instance FromJSON User

data SignupRequest = SignupRequest
  { signupUsername :: Text
  , signupEmail    :: Text
  , signupPassword :: Text
  } deriving (Generic)

signupFieldModifier :: String -> String
signupFieldModifier = map toLower . drop (length "signup")

instance ToJSON SignupRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = signupFieldModifier }
instance FromJSON SignupRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = signupFieldModifier }

data LoginRequest = LoginRequest
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Generic)

instance ToJSON LoginRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop (length "login") }
instance FromJSON LoginRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop (length "login") }
