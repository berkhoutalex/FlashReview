{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module API(FlashcardAPI, Flashcard(..), ReviewResult(..), Stats(..), User(..)) where

import           Data.Aeson          (FromJSON, ToJSON)
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
  } deriving (Generic, Show)

instance ToJSON Flashcard
instance FromJSON Flashcard

type FlashcardAPI auths =
       Auth auths User :> "cards" :> Get '[JSON] [Flashcard]
  :<|> Auth auths User :> "cards" :> ReqBody '[JSON] Flashcard :> Post '[JSON] Flashcard
  :<|> Auth auths User :> "cards" :> Capture "id" UUID :> ReqBody '[JSON] Flashcard :> Put '[JSON] Flashcard
  :<|> Auth auths User :> "cards" :> Capture "id" UUID :> DeleteNoContent
  :<|> Auth auths User :> "review" :> "queue" :> Get '[JSON] [Flashcard]
  :<|> Auth auths User :> "review" :> Capture "id" UUID :> ReqBody '[JSON] ReviewResult :> PostNoContent
  :<|> Auth auths User :> "stats" :> Get '[JSON] Stats
  :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
  :<|> "signup" :> ReqBody '[JSON] User :> Post '[JSON] API.User

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
  } deriving (Generic, FromJWT, ToJWT)

instance ToJSON User
instance FromJSON User
