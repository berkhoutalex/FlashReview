module Components.FlashcardList where

import Prelude

import API (Flashcard(..), getAllCards, deleteCard)
import Components.FlashcardForm as FlashcardForm
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Cursor (pointer)
import Data.Array (null)
import Type.Proxy (Proxy(..))
type State =
  { cards :: Array Flashcard
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | Refresh
  | DeleteCard Flashcard
  | HandleFormOutput

type Slots =
  ( flashcardForm :: forall query. H.Slot query Unit Unit )

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> 
      { cards: []
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = 
  HH.div
    [ HCSS.style do
        CSS.margin (CSS.px 20.0) (CSS.px 0.0) (CSS.px 20.0) (CSS.px 0.0)
    ]
    [ HH.h2 
        [ HCSS.style do
            CSS.color (CSS.rgb 33 150 243)
        ]
        [ HH.text "Flashcards" ]
    , HH.slot (Proxy :: _ "flashcardForm") unit FlashcardForm.component unit (const HandleFormOutput)
    , if state.loading
        then HH.div_ [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div 
                        [ HCSS.style do
                            CSS.color (CSS.rgb 220 0 0)
                        ] 
                        [ HH.text $ "Error: " <> err ]
          Nothing -> renderCardList state.cards
    , HH.button
        [ HE.onClick \_ -> Refresh
        , HCSS.style do
            CSS.marginTop (CSS.px 20.0)
            CSS.padding (CSS.px 8.0) (CSS.px 16.0) (CSS.px 8.0) (CSS.px 16.0)
            CSS.backgroundColor (CSS.rgb 33 150 243)
            CSS.color (CSS.rgb 255 255 255)
            CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 33 150 243)
            CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
            CSS.cursor pointer
        ]
        [ HH.text "Refresh" ]
    ]

renderCardList :: forall m. MonadAff m => Array Flashcard -> H.ComponentHTML Action Slots m
renderCardList cards =
  if null cards
    then HH.div_ [ HH.text "No flashcards found." ]
    else HH.div_ $ map renderCard cards

renderCard :: forall m. MonadAff m => Flashcard -> H.ComponentHTML Action Slots m
renderCard card@(Flashcard c) =
  HH.div
    [ HCSS.style do
        CSS.margin (CSS.px 0.0) (CSS.px 0.0) (CSS.px 16.0) (CSS.px 0.0)
        CSS.padding (CSS.px 16.0) (CSS.px 16.0) (CSS.px 16.0) (CSS.px 16.0)
        CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 200 200 200)
        CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
        CSS.backgroundColor (CSS.rgb 250 250 250)
    ]
    [ HH.div_ [ HH.text $ "Front: " <> c.front ]
    , HH.div_ [ HH.text $ "Back: " <> c.back ]
    , HH.div_ [ HH.text $ "Repetitions: " <> show c.repetitions ]
    , HH.button
        [ HE.onClick \_ -> DeleteCard card
        , HCSS.style do
            CSS.marginTop (CSS.px 10.0)
            CSS.padding (CSS.px 6.0) (CSS.px 12.0) (CSS.px 6.0) (CSS.px 12.0)
            CSS.backgroundColor (CSS.rgb 220 53 69)
            CSS.color (CSS.rgb 255 255 255)
            CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 220 53 69)
            CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
            CSS.cursor pointer
        ]
        [ HH.text "Delete" ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction Refresh

  Refresh -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff getAllCards
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right cards -> H.modify_ \s -> s { loading = false, cards = cards }

  DeleteCard (Flashcard card) -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff $ deleteCard card.id
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right _ -> handleAction Refresh
      
  HandleFormOutput -> do
    handleAction Refresh
