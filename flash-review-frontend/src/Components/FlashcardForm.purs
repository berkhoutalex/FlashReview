module Components.FlashcardForm where

import Prelude

import API (Flashcard(..), createCard, wrapUUID, wrapDateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { front :: String
  , back :: String
  , submitting :: Boolean
  , error :: Maybe String
  , success :: Boolean
  }

data Action
  = UpdateFront String
  | UpdateBack String
  | SubmitForm
  | ResetForm

type Output = Unit

component :: forall q i m. MonadAff m => H.Component q i Output m
component = H.mkComponent
  { initialState: \_ ->
      { front: ""
      , back: ""
      , submitting: false
      , error: Nothing
      , success: false
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "card") ]
    [ HH.h3 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Add New Flashcard" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-row") ]
        [ formField "Front side" state.front UpdateFront
        , formField "Back side" state.back UpdateBack
        ]
    , if state.submitting
        then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Submitting..." ]
        else HH.div_ []
    , case state.error of
        Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
        Nothing -> HH.div_ []
    , if state.success
        then HH.div [ HP.class_ (HH.ClassName "alert alert-success") ] [ HH.text "Card created successfully!" ]
        else HH.div_ []
    , HH.div
        [ HP.class_ (HH.ClassName "form-actions") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "btn btn-secondary")
            , HE.onClick \_ -> ResetForm
            ]
            [ HH.text "Reset" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-primary")
            , HE.onClick \_ -> SubmitForm
            , HP.disabled (state.front == "" || state.back == "" || state.submitting)
            ]
            [ HH.text "Create Card" ]
        ]
    ]
  where
    formField label value updateAction =
      HH.div
        [ HP.class_ (HH.ClassName "field") ]
        [ HH.label [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.textarea
            [ HP.class_ (HH.ClassName "textarea")
            , HP.value value
            , HE.onValueInput updateAction
            ]
        ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  UpdateFront front -> do
    H.modify_ \s -> s { front = front, success = false }

  UpdateBack back -> do
    H.modify_ \s -> s { back = back, success = false }

  ResetForm -> do
    H.modify_ \s -> s
      { front = ""
      , back = ""
      , error = Nothing
      , success = false
      }

  SubmitForm -> do
    state <- H.get
    H.modify_ \s -> s { submitting = true, error = Nothing, success = false }

    now <- H.liftEffect nowDateTime
    uuid <- H.liftEffect genUUID

    let newCard = Flashcard
          { id: wrapUUID uuid
          , front: state.front
          , back: state.back
          , nextReview: wrapDateTime now
          , interval: 1
          , easeFactor: 2.5
          , repetitions: 0
          }

    result <- H.liftAff $ createCard newCard

    case result of
      Left err -> H.modify_ \s -> s { submitting = false, error = Just err }
      Right _ -> do
        H.modify_ \s -> s
          { submitting = false
          , success = true
          , front = ""
          , back = ""
          }
        H.raise unit
