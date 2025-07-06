module Components.Stats where

import Prelude

import API (Stats(..), getStats)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Cursor (pointer)

type State =
  { stats :: Maybe Stats
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | RefreshStats

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> 
      { stats: Nothing
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
        [ HH.text "Statistics" ]
    , if state.loading
        then HH.div_ [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div 
                        [ HCSS.style do
                            CSS.color (CSS.rgb 220 0 0)
                        ] 
                        [ HH.text $ "Error: " <> err ]
          Nothing -> renderStats state.stats
    , HH.button
        [ HE.onClick \_ -> RefreshStats
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

renderStats :: forall m. Maybe Stats -> H.ComponentHTML Action () m
renderStats Nothing = HH.div_ [ HH.text "No stats available." ]
renderStats (Just (Stats s)) =
  HH.div
    [ HCSS.style do
        CSS.padding (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0)
        CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 200 200 200)
        CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
        CSS.backgroundColor (CSS.rgb 250 250 250)
    ]
    [ HH.div 
        [ HCSS.style do
            CSS.fontSize (CSS.px 18.0)
            CSS.marginBottom (CSS.px 10.0)
        ]
        [ HH.text $ "Cards due today: " <> show s.dueToday ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction RefreshStats

  RefreshStats -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff getStats
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right stats -> H.modify_ \s -> s { loading = false, stats = Just stats }
