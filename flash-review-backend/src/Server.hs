{-# LANGUAGE DataKinds       #-}

{-# LANGUAGE RecordWildCards #-}


module Server( server, initializeApp ) where

import qualified API                        (Flashcard, FlashcardAPI,
                                             ReviewResult, Stats (..))
import           Control.Monad.IO.Class     (liftIO)
import           Data.UUID                  (UUID)
import qualified Database                   as DB
import qualified Database.PostgreSQL.Simple as PG
import           Servant

newtype AppEnv = AppEnv
  { appDbConn :: PG.Connection
  }

initializeApp :: IO AppEnv
initializeApp = do
  conn <- DB.connectDb
  DB.setupSchema conn
  pure $ AppEnv conn


server :: AppEnv -> Server API.FlashcardAPI
server env =
       getCards env
  :<|> createCard env
  :<|> updateCard env
  :<|> deleteCard env
  :<|> getReviewQueue env
  :<|> submitReview env
  :<|> getStats env


getCards :: AppEnv -> Handler [API.Flashcard]
getCards AppEnv{..} = liftIO $ DB.getAllCardsDb appDbConn

createCard :: AppEnv -> API.Flashcard -> Handler API.Flashcard
createCard AppEnv{..} flashcard = liftIO $ DB.createCardDb appDbConn flashcard

updateCard :: AppEnv -> UUID -> API.Flashcard -> Handler API.Flashcard
updateCard AppEnv{..} uuid flashcard = liftIO $ DB.updateCardDb appDbConn uuid flashcard

deleteCard :: AppEnv -> UUID -> Handler NoContent
deleteCard AppEnv{..} uuid = do
  liftIO $ DB.deleteCardDb appDbConn uuid
  pure NoContent

getReviewQueue :: AppEnv -> Handler [API.Flashcard]
getReviewQueue AppEnv{..} = liftIO $ DB.getReviewCardsDb appDbConn

submitReview :: AppEnv -> UUID -> API.ReviewResult -> Handler NoContent
submitReview AppEnv{..} uuid result = do
  liftIO $ DB.processReviewDb appDbConn uuid result
  pure NoContent

getStats :: AppEnv -> Handler API.Stats
getStats AppEnv{..} = do
  dueCount <- liftIO $ DB.getDueCountDb appDbConn
  pure $ API.Stats dueCount
