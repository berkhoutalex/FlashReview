module Components.Stats where

import Prelude

import API (Stats(..), getStats)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
  HH.div_
    [ HH.div
        [ HP.class_ (HH.ClassName "page-heading-row") ]
        [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Statistics" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-icon")
            , HE.onClick \_ -> RefreshStats
            ]
            [ HH.text "↻" ]
        ]
    , if state.loading
        then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
          Nothing -> renderStats state.stats
    ]

renderStats :: forall m. Maybe Stats -> H.ComponentHTML Action () m
renderStats Nothing = HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "No stats available." ]
renderStats (Just (Stats s)) =
  HH.div
    [ HP.class_ (HH.ClassName "card stat-tile") ]
    [ HH.div [ HP.class_ (HH.ClassName "stat-tile-value") ] [ HH.text $ show s.dueToday ]
    , HH.div [ HP.class_ (HH.ClassName "stat-tile-label") ] [ HH.text "Cards due today" ]
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
