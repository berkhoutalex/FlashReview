

module Server( server ) where

import           API       (Flashcard, FlashcardAPI, ReviewResult, Stats (..))
import           Data.UUID (UUID)
import           Servant

server :: Server API.FlashcardAPI
server =
       getCards
  :<|> createCard
  :<|> updateCard
  :<|> deleteCard
  :<|> getReviewQueue
  :<|> submitReview
  :<|> getStats

getCards :: Handler [Flashcard]
getCards = pure []

createCard :: Flashcard -> Handler Flashcard
createCard = pure

updateCard :: UUID -> Flashcard -> Handler Flashcard
updateCard _ = pure

deleteCard :: UUID -> Handler NoContent
deleteCard _ = pure NoContent

getReviewQueue :: Handler [Flashcard]
getReviewQueue = pure []

submitReview :: UUID -> ReviewResult -> Handler NoContent
submitReview _ _ = pure NoContent

getStats :: Handler Stats
getStats = pure $ Stats 0
