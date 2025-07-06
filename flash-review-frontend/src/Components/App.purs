module Components.App where

import Prelude

import Components.FlashcardList as FlashcardList
import Components.Review as Review
import Components.Stats as Stats
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Cursor (pointer)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

data View = FlashcardsView | ReviewView | StatsView

derive instance eqView :: Eq View



type State = 
  { currentView :: View
  }

data Action = SwitchView View


type Slots =
  ( flashcardList :: forall query. H.Slot query Unit Unit
  , review :: forall query. H.Slot query Unit Unit
  , stats :: forall query. H.Slot query Unit Unit
  )


component :: forall query input output. H.Component query input output Aff
component =
  H.mkComponent
    { initialState: const { currentView: FlashcardsView }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HCSS.style do
        CSS.display CSS.flex
        CSS.flexDirection CSS.column
        CSS.height (CSS.vh 100.0)
        CSS.width (CSS.vw 100.0)
    ]
    [ renderHeader state
    , renderContent state
    ]

renderHeader :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderHeader state =
  HH.div
    [ HCSS.style do
        CSS.padding (CSS.px 20.0) (CSS.px 30.0) (CSS.px 20.0) (CSS.px 30.0)
        CSS.backgroundColor (CSS.rgb 33 150 243)
        CSS.color (CSS.rgb 255 255 255)
        CSS.display CSS.flex
        CSS.flexDirection CSS.row
        CSS.justifyContent CSS.spaceBetween
    ]
    [ HH.h1
        [ HCSS.style do
            CSS.margin (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
            CSS.fontSize (CSS.px 24.0)
        ]
        [ HH.text "Flash Review" ]
    , HH.div
        [ HCSS.style do
            CSS.display CSS.flex
            CSS.flexDirection CSS.row
        ]
        [ navLink FlashcardsView "Flashcards" state.currentView
        , navLink ReviewView "Review" state.currentView
        , navLink StatsView "Stats" state.currentView
        ]
    ]

navLink :: forall m. MonadAff m => View -> String -> View -> H.ComponentHTML Action Slots m
navLink view label currentView =
  HH.a
    [ HE.onClick \_ -> SwitchView view
    , HCSS.style do
        CSS.padding (CSS.px 0.0) (CSS.px 15.0) (CSS.px 0.0) (CSS.px 15.0)
        CSS.color (CSS.rgb 255 255 255)
        CSS.cursor (pointer)
        if view == currentView
          then do
            CSS.fontWeight CSS.bold
            CSS.textDecoration CSS.underline
          else
            CSS.textDecoration CSS.noneTextDecoration
    ]
    [ HH.text label ]

renderContent :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderContent state =
  HH.div
    [ HCSS.style do
        CSS.padding (CSS.px 20.0) (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0)
        CSS.backgroundColor (CSS.rgb 250 250 255)
    ]
    [ case state.currentView of
        FlashcardsView -> HH.slot_ (Proxy :: _ "flashcardList") unit FlashcardList.component unit
        ReviewView -> HH.slot_ (Proxy :: _ "review") unit Review.component unit
        StatsView -> HH.slot_ (Proxy :: _ "stats") unit Stats.component unit
    ]

handleAction :: forall m output. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchView view -> H.modify_ \s -> s { currentView = view }


