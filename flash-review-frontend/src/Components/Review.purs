module Components.Review where

import Prelude

import API (Flashcard(..), ReviewResult(..), getReviewQueue, submitReview)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Cursor (pointer)
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
  HH.div
    [ HCSS.style do
        CSS.margin (CSS.px 20.0) (CSS.px 0.0) (CSS.px 20.0) (CSS.px 0.0)
    ]
    [ HH.h2 
        [ HCSS.style do
            CSS.color (CSS.rgb 33 150 243)
        ]
        [ HH.text "Review Cards" ]
    , if state.loading
        then HH.div_ [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div 
                        [ HCSS.style do
                            CSS.color (CSS.rgb 220 0 0)
                        ] 
                        [ HH.text $ "Error: " <> err ]
          Nothing -> 
            if null state.queue
              then HH.div_ [ HH.text "No cards to review!" ]
              else renderReview state
    ]

renderReview :: forall m. State -> H.ComponentHTML Action () m
renderReview state =
  let
    currentCard = state.queue !! state.currentIndex
  in
    case currentCard of
      Nothing -> HH.div_ [ HH.text "Review complete!" ]
      Just (Flashcard card) -> 
        HH.div
          [ HCSS.style do
              CSS.padding (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0)
              CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 200 200 200)
              CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
              CSS.backgroundColor (CSS.rgb 250 250 250)
          ]
          [ HH.div 
              [ HCSS.style do
                  CSS.fontSize (CSS.px 24.0)
                  CSS.marginBottom (CSS.px 20.0)
              ] 
              [ HH.text card.front ]
          , if state.showAnswer
              then 
                HH.div_
                  [ HH.div 
                      [ HCSS.style do
                          CSS.fontSize (CSS.px 24.0)
                          CSS.marginTop (CSS.px 20.0)
                          CSS.marginBottom (CSS.px 30.0)
                          CSS.padding (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0) (CSS.px 15.0)
                          CSS.backgroundColor (CSS.rgb 240 240 240)
                          CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 230 230 230)
                          CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                      ] 
                      [ HH.text card.back ]
                  , HH.div
                      [ HCSS.style do
                          CSS.display CSS.flex
                          CSS.justifyContent CSS.spaceAround
                          CSS.marginTop (CSS.px 20.0)
                      ]
                      [ ratingButton 1 "1 - Hard"
                      , ratingButton 3 "3 - Good"
                      , ratingButton 5 "5 - Easy"
                      ]
                  ]
              else 
                HH.button
                  [ HE.onClick \_ -> ShowAnswer
                  , HCSS.style do
                      CSS.marginTop (CSS.px 20.0)
                      CSS.padding (CSS.px 10.0) (CSS.px 20.0) (CSS.px 10.0) (CSS.px 20.0)
                      CSS.backgroundColor (CSS.rgb 33 150 243)
                      CSS.color (CSS.rgb 255 255 255)
                      CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 33 150 243)
                      CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                      CSS.cursor pointer
                  ]
                  [ HH.text "Show Answer" ]
          ]
  where
    ratingButton rating label =
      HH.button
        [ HE.onClick \_ -> SubmitRating rating
        , HCSS.style do
            CSS.padding (CSS.px 8.0) (CSS.px 16.0) (CSS.px 8.0) (CSS.px 16.0)
            CSS.backgroundColor (CSS.rgb 33 150 243)
            CSS.color (CSS.rgb 255 255 255)
            CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 33 150 243)
            CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
            CSS.cursor pointer
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
