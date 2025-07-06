{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API(FlashcardAPI, Flashcard(..), ReviewResult(..), Stats(..)) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID       (UUID)
import           GHC.Generics    (Generic)
import           Servant

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

type FlashcardAPI =
       "cards" :> Get '[JSON] [Flashcard]
  :<|> "cards" :> ReqBody '[JSON] Flashcard :> Post '[JSON] Flashcard
  :<|> "cards" :> Capture "id" UUID :> ReqBody '[JSON] Flashcard :> Put '[JSON] Flashcard
  :<|> "cards" :> Capture "id" UUID :> DeleteNoContent
  :<|> "review" :> "queue" :> Get '[JSON] [Flashcard]
  :<|> "review" :> Capture "id" UUID :> ReqBody '[JSON] ReviewResult :> PostNoContent
  :<|> "stats" :> Get '[JSON] Stats

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
