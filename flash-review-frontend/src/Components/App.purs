module Components.App where

import Prelude

import CSS as CSS
import CSS.Cursor (pointer)
import Components.FlashcardList as FlashcardList
import Components.Login as Login
import Components.Review as Review
import Components.Signup as Signup
import Components.Stats as Stats
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

data View = FlashcardsView | ReviewView | StatsView | LoginView | SignupView

derive instance eqView :: Eq View



type State = 
  { currentView :: View
  , isLoggedIn :: Boolean
  }

data Action 
  = SwitchView View
  | HandleLoginMessage Login.LoginOutput
  | HandleSignupMessage Signup.SignupOutput
  | Logout

type Slots =
  ( flashcardList :: forall query. H.Slot query Unit Unit
  , review :: forall query. H.Slot query Unit Unit
  , stats :: forall query. H.Slot query Unit Unit
  , login :: H.Slot Query Login.LoginOutput Unit
  , signup :: H.Slot Query Signup.SignupOutput Unit
  )

data Query a
  = IsLoggedIn (Boolean -> a)


component :: forall input output. H.Component Query input output Aff
component =
  H.mkComponent
    { initialState: const { currentView: LoginView, isLoggedIn: false }
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
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
  where
    renderContent :: MonadAff m => State -> H.ComponentHTML Action Slots m
    renderContent st = 
      if not st.isLoggedIn && st.currentView /= LoginView && st.currentView /= SignupView
        then renderAuthView st
        else renderMainContent st
        
    renderAuthView :: MonadAff m => State -> H.ComponentHTML Action Slots m
    renderAuthView st = case st.currentView of
      LoginView -> HH.slot_ _login unit Login.component unit
      SignupView -> HH.slot_ _signup unit Signup.component unit
      _ -> renderLoginComponent

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
    , if state.isLoggedIn
        then 
          HH.div
            [ HCSS.style do
                CSS.display CSS.flex
                CSS.flexDirection CSS.row
            ]
            [ navLink FlashcardsView "Flashcards" state.currentView
            , navLink ReviewView "Review" state.currentView
            , navLink StatsView "Stats" state.currentView
            , HH.a
                [ HE.onClick \_ -> Logout
                , HCSS.style do
                    CSS.padding (CSS.px 0.0) (CSS.px 15.0) (CSS.px 0.0) (CSS.px 15.0)
                    CSS.color (CSS.rgb 255 255 255)
                    CSS.cursor (pointer)
                    CSS.marginLeft (CSS.px 10.0)
                ]
                [ HH.text "Logout" ]
            ]
        else if state.currentView == LoginView 
          then 
            HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.flexDirection CSS.row
              ]
              [ navLink SignupView "Sign Up" state.currentView ]
          else 
            HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.flexDirection CSS.row
              ]
              [ navLink LoginView "Login" state.currentView ]
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

renderLoginComponent :: forall m. MonadAff m => H.ComponentHTML Action Slots m
renderLoginComponent = 
  HH.slot _login unit Login.component unit HandleLoginMessage
  
renderSignupComponent :: forall m. MonadAff m => H.ComponentHTML Action Slots m
renderSignupComponent =
  HH.slot _signup unit Signup.component unit HandleSignupMessage

_login = Proxy :: Proxy "login"
_signup = Proxy :: Proxy "signup"
_flashcardList = Proxy :: Proxy "flashcardList"
_review = Proxy :: Proxy "review"
_stats = Proxy :: Proxy "stats"

renderMainContent :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderMainContent state = 
  HH.div
    [ HCSS.style do

        CSS.padding (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0) (CSS.px 20.0)
    ]
    [ case state.currentView of
        FlashcardsView -> HH.slot_ _flashcardList unit FlashcardList.component unit
        ReviewView -> HH.slot_ _review unit Review.component unit
        StatsView -> HH.slot_ _stats unit Stats.component unit
        LoginView -> renderLoginComponent
        SignupView -> renderSignupComponent
    ]

handleAction :: forall m output. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchView view -> 
    H.modify_ \st -> st { currentView = view }
  
  HandleLoginMessage msg -> case msg of
    Login.LoginSuccessful -> do
      H.modify_ \st -> st { isLoggedIn = true, currentView = FlashcardsView }
    
    Login.GoToSignup -> 
      H.modify_ \st -> st { currentView = SignupView }
  
  HandleSignupMessage msg -> case msg of
    Signup.SignupSuccessful -> do
      H.modify_ \st -> st { currentView = LoginView }
    
    Signup.GoToLogin ->
      H.modify_ \st -> st { currentView = LoginView }
  
  Logout ->
    H.modify_ \st -> st { isLoggedIn = false, currentView = LoginView }

handleQuery :: forall a m output. MonadAff m => Query a -> H.HalogenM State Action Slots output m (Maybe a)
handleQuery = case _ of
  IsLoggedIn reply -> do
    state <- H.get
    pure $ Just (reply state.isLoggedIn)

