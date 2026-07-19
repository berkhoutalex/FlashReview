module Components.App where

import Prelude

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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Theme as Theme
import Type.Proxy (Proxy(..))

data View = FlashcardsView | ReviewView | StatsView | LoginView | SignupView

derive instance eqView :: Eq View

type State =
  { currentView :: View
  , isLoggedIn :: Boolean
  , theme :: String
  , sidebarOpen :: Boolean
  }

data Action
  = SwitchView View
  | HandleLoginMessage Login.LoginOutput
  | HandleSignupMessage Signup.SignupOutput
  | Logout
  | ToggleSidebar
  | CloseSidebar
  | ToggleTheme

type Slots =
  ( flashcardList :: forall query. H.Slot query Unit Unit
  , review :: forall query. H.Slot query Unit Unit
  , stats :: forall query. H.Slot query Unit Unit
  , login :: H.Slot Query Login.LoginOutput Unit
  , signup :: H.Slot Query Signup.SignupOutput Unit
  )

data Query a
  = IsLoggedIn (Boolean -> a)

component :: forall output. H.Component Query String output Aff
component =
  H.mkComponent
    { initialState: \initialTheme ->
        { currentView: LoginView
        , isLoggedIn: false
        , theme: initialTheme
        , sidebarOpen: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  if state.isLoggedIn
    then renderAppShell state
    else renderAuthShell state

renderAppShell :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderAppShell state =
  HH.div_
    [ HH.div
        [ HP.class_ (HH.ClassName "topbar") ]
        [ HH.div [ HP.class_ (HH.ClassName "sidebar-wordmark") ] [ HH.text "FlashReview" ]
        , HH.button
            [ HP.class_ (HH.ClassName "hamburger")
            , HE.onClick \_ -> ToggleSidebar
            ]
            [ HH.text "☰" ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName ("drawer-backdrop" <> if state.sidebarOpen then " is-open" else ""))
        , HE.onClick \_ -> CloseSidebar
        ]
        []
    , HH.div
        [ HP.class_ (HH.ClassName "app-shell") ]
        [ renderSidebar state
        , HH.div
            [ HP.class_ (HH.ClassName "main-content") ]
            [ renderMainContent state ]
        ]
    ]

renderSidebar :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderSidebar state =
  HH.div
    [ HP.class_ (HH.ClassName ("sidebar" <> if state.sidebarOpen then " is-open" else "")) ]
    [ HH.div [ HP.class_ (HH.ClassName "sidebar-wordmark") ] [ HH.text "FlashReview" ]
    , HH.div
        [ HP.class_ (HH.ClassName "sidebar-nav") ]
        [ navLink "⚡" ReviewView "Review" state.currentView
        , navLink "🗂" FlashcardsView "Flashcards" state.currentView
        , navLink "📊" StatsView "Stats" state.currentView
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "sidebar-footer") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "nav-link")
            , HE.onClick \_ -> ToggleTheme
            ]
            [ HH.text $ if state.theme == "dark" then "☀ Light mode" else "🌙 Dark mode" ]
        , HH.a
            [ HP.class_ (HH.ClassName "nav-link")
            , HE.onClick \_ -> Logout
            ]
            [ HH.text "Logout" ]
        ]
    ]

navLink :: forall m. MonadAff m => String -> View -> String -> View -> H.ComponentHTML Action Slots m
navLink icon view label currentView =
  HH.a
    [ HP.class_ (HH.ClassName ("nav-link" <> if view == currentView then " is-active" else ""))
    , HE.onClick \_ -> SwitchView view
    ]
    [ HH.text (icon <> "  " <> label) ]

renderAuthShell :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderAuthShell state =
  HH.div
    [ HP.class_ (HH.ClassName "auth-shell") ]
    [ HH.div [ HP.class_ (HH.ClassName "auth-wordmark") ] [ HH.text "FlashReview" ]
    , case state.currentView of
        SignupView -> HH.slot _signup unit Signup.component unit HandleSignupMessage
        _ -> HH.slot _login unit Login.component unit HandleLoginMessage
    ]

renderMainContent :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderMainContent state =
  case state.currentView of
    FlashcardsView -> HH.slot_ _flashcardList unit FlashcardList.component unit
    ReviewView -> HH.slot_ _review unit Review.component unit
    StatsView -> HH.slot_ _stats unit Stats.component unit
    LoginView -> HH.slot _login unit Login.component unit HandleLoginMessage
    SignupView -> HH.slot _signup unit Signup.component unit HandleSignupMessage

_login = Proxy :: Proxy "login"
_signup = Proxy :: Proxy "signup"
_flashcardList = Proxy :: Proxy "flashcardList"
_review = Proxy :: Proxy "review"
_stats = Proxy :: Proxy "stats"

handleAction :: forall m output. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchView view ->
    H.modify_ \st -> st { currentView = view, sidebarOpen = false }

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
    H.modify_ \st -> st { isLoggedIn = false, currentView = LoginView, sidebarOpen = false }

  ToggleSidebar ->
    H.modify_ \st -> st { sidebarOpen = not st.sidebarOpen }

  CloseSidebar ->
    H.modify_ \st -> st { sidebarOpen = false }

  ToggleTheme -> do
    state <- H.get
    let newTheme = if state.theme == "dark" then "light" else "dark"
    H.liftEffect $ Theme.setTheme newTheme
    H.modify_ \st -> st { theme = newTheme }

handleQuery :: forall a m output. MonadAff m => Query a -> H.HalogenM State Action Slots output m (Maybe a)
handleQuery = case _ of
  IsLoggedIn reply -> do
    state <- H.get
    pure $ Just (reply state.isLoggedIn)
