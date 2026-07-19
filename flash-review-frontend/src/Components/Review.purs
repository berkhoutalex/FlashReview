module Components.Review where

import Prelude

import API (Flashcard(..), ReviewResult(..), getReviewQueue, submitReview)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array ((!!), length, null)

type State =
  { queue :: Array Flashcard
  , currentIndex :: Int
  , showAnswer :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | LoadReviewQueue
  | ShowAnswer
  | SubmitRating Int
  | NextCard

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { queue: []
      , currentIndex: 0
      , showAnswer: false
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Review Cards" ]
    , if state.loading
        then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
          Nothing ->
            if null state.queue
              then HH.div [ HP.class_ (HH.ClassName "review-empty") ] [ HH.text "No cards to review!" ]
              else renderReview state
    ]

renderReview :: forall m. State -> H.ComponentHTML Action () m
renderReview state =
  let
    currentCard = state.queue !! state.currentIndex
  in
    case currentCard of
      Nothing -> HH.div [ HP.class_ (HH.ClassName "review-empty") ] [ HH.text "Review complete!" ]
      Just (Flashcard card) ->
        HH.div_
          [ HH.div
              [ HP.class_ (HH.ClassName "review-progress") ]
              [ HH.text $ show (state.currentIndex + 1) <> " of " <> show (length state.queue) ]
          , HH.div
              [ HP.class_ (HH.ClassName "card review-card") ]
              [ HH.div [ HP.class_ (HH.ClassName "review-front") ] [ HH.text card.front ]
              , if state.showAnswer
                  then
                    HH.div_
                      [ HH.hr [ HP.class_ (HH.ClassName "review-divider") ]
                      , HH.div [ HP.class_ (HH.ClassName "review-back") ] [ HH.text card.back ]
                      , HH.div
                          [ HP.class_ (HH.ClassName "review-ratings") ]
                          [ ratingButton "btn-danger-outline" 1 "Hard"
                          , ratingButton "btn-secondary" 3 "Good"
                          , ratingButton "btn-success-outline" 5 "Easy"
                          ]
                      ]
                  else
                    HH.button
                      [ HP.class_ (HH.ClassName "btn btn-primary btn-block")
                      , HE.onClick \_ -> ShowAnswer
                      ]
                      [ HH.text "Show Answer" ]
              ]
          ]
  where
    ratingButton variantClass rating label =
      HH.button
        [ HP.class_ (HH.ClassName ("btn " <> variantClass))
        , HE.onClick \_ -> SubmitRating rating
        ]
        [ HH.text label ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadReviewQueue

  LoadReviewQueue -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff getReviewQueue
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right cards -> H.modify_ \s -> s { loading = false, queue = cards, currentIndex = 0, showAnswer = false }

  ShowAnswer -> do
    H.modify_ \s -> s { showAnswer = true }

  SubmitRating rating -> do
    state <- H.get
    case state.queue !! state.currentIndex of
      Nothing -> pure unit
      Just (Flashcard card) -> do
        H.modify_ \s -> s { loading = true, error = Nothing }
        result <- H.liftAff $ submitReview card.id (ReviewResult { rating })
        case result of
          Left err -> H.modify_ \s -> s { loading = false, error = Just err }
          Right _ -> handleAction NextCard

  NextCard -> do
    state <- H.get
    let nextIndex = state.currentIndex + 1
    if nextIndex >= length state.queue
      then H.modify_ \s -> s { loading = false, queue = [], currentIndex = 0, showAnswer = false }
      else H.modify_ \s -> s { loading = false, currentIndex = nextIndex, showAnswer = false }
