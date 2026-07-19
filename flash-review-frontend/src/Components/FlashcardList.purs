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
import Halogen.HTML.Properties as HP
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
  HH.div_
    [ HH.div
        [ HP.class_ (HH.ClassName "page-heading-row") ]
        [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Flashcards" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-icon")
            , HE.onClick \_ -> Refresh
            ]
            [ HH.text "↻" ]
        ]
    , HH.slot (Proxy :: _ "flashcardForm") unit FlashcardForm.component unit (const HandleFormOutput)
    , HH.div
        [ HP.class_ (HH.ClassName "section") ]
        [ if state.loading
            then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Loading..." ]
            else case state.error of
              Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
              Nothing -> renderCardList state.cards
        ]
    ]

renderCardList :: forall m. MonadAff m => Array Flashcard -> H.ComponentHTML Action Slots m
renderCardList cards =
  if null cards
    then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "No flashcards found." ]
    else HH.div [ HP.class_ (HH.ClassName "flashcard-grid") ] $ map renderCard cards

renderCard :: forall m. MonadAff m => Flashcard -> H.ComponentHTML Action Slots m
renderCard card@(Flashcard c) =
  HH.div
    [ HP.class_ (HH.ClassName "flashcard-tile") ]
    [ HH.div [ HP.class_ (HH.ClassName "flashcard-tile-front") ] [ HH.text c.front ]
    , HH.div [ HP.class_ (HH.ClassName "flashcard-tile-back") ] [ HH.text c.back ]
    , HH.div
        [ HP.class_ (HH.ClassName "flashcard-tile-footer") ]
        [ HH.span [ HP.class_ (HH.ClassName "badge") ] [ HH.text $ show c.repetitions <> " reps" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-icon flashcard-tile-delete")
            , HE.onClick \_ -> DeleteCard card
            ]
            [ HH.text "🗑" ]
        ]
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
