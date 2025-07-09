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
import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Cursor (pointer, notAllowed)
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
    [ HCSS.style do
        CSS.margin (CSS.px 0.0) (CSS.px 0.0) (CSS.px 30.0) (CSS.px 0.0)
        CSS.padding (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0)
        CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 200 200 200)
        CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
        CSS.backgroundColor (CSS.rgb 250 250 250)
    ]
    [ HH.h3 
        [ HCSS.style do
            CSS.color (CSS.rgb 33 150 243)
            CSS.marginTop (CSS.px 0.0)
        ]
        [ HH.text "Add New Flashcard" ]
    , formField "Front side:" state.front UpdateFront
    , formField "Back side:" state.back UpdateBack
    , if state.submitting
        then HH.div_ [ HH.text "Submitting..." ]
        else HH.div_ []
    , case state.error of
        Just err -> HH.div 
                      [ HCSS.style do
                          CSS.color (CSS.rgb 220 0 0)
                          CSS.marginTop (CSS.px 10.0)
                      ] 
                      [ HH.text $ "Error: " <> err ]
        Nothing -> HH.div_ []
    , if state.success
        then HH.div 
              [ HCSS.style do
                  CSS.color (CSS.rgb 40 167 69)
                  CSS.marginTop (CSS.px 10.0)
              ] 
              [ HH.text "Card created successfully!" ]
        else HH.div_ []
    , HH.div
        [ HCSS.style do
            CSS.marginTop (CSS.px 20.0)
            CSS.display CSS.flex
            CSS.justifyContent CSS.flexEnd
        ]
        [ HH.button
            [ HE.onClick \_ -> ResetForm
            , HCSS.style do
                CSS.padding (CSS.px 8.0) (CSS.px 16.0) (CSS.px 8.0) (CSS.px 16.0)
                CSS.backgroundColor (CSS.rgb 108 117 125)
                CSS.color (CSS.rgb 255 255 255)
                CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 108 117 125)
                CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                CSS.cursor pointer
                CSS.marginRight (CSS.px 10.0)
            ]
            [ HH.text "Reset" ]
        , HH.button
            [ HE.onClick \_ -> SubmitForm
            , HP.disabled (state.front == "" || state.back == "" || state.submitting)
            , HCSS.style do
                CSS.padding (CSS.px 8.0) (CSS.px 16.0) (CSS.px 8.0) (CSS.px 16.0)
                CSS.backgroundColor (CSS.rgb 33 150 243)
                CSS.color (CSS.rgb 255 255 255)
                CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 33 150 243)
                CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                CSS.cursor pointer
                when (state.front == "" || state.back == "" || state.submitting) do
                  CSS.opacity 0.6
                  CSS.cursor notAllowed
            ]
            [ HH.text "Create Card" ]
        ]
    ]
  where
    formField label value updateAction =
      HH.div
        [ HCSS.style do
            CSS.marginBottom (CSS.px 15.0)
        ]
        [ HH.label
            [ HCSS.style do
                CSS.display CSS.block
                CSS.marginBottom (CSS.px 5.0)
            ]
            [ HH.text label ]
        , HH.textarea
            [ HP.value value
            , HE.onValueInput updateAction
            , HCSS.style do
                CSS.display CSS.block
                CSS.width (CSS.pct 100.0)
                CSS.padding (CSS.px 8.0) (CSS.px 12.0) (CSS.px 8.0) (CSS.px 12.0)
                CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 200 200 200)
                CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                CSS.minHeight (CSS.px 100.0)
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
