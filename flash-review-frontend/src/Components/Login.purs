module Components.Login where

import Prelude

import API.Client as Client
import API.Types (UserCredentials(..))
import CSS (backgroundColor, block, border, borderBox, borderRadius, boxSizing, color, column, display, flex, flexDirection, fontSize, height, marginBottom, marginTop, padding, pct, px, rgb, solid, textDecoration, underline, vh, width) as CSS
import CSS.Cursor (cursor, pointer) as CSS
import CSS.TextAlign (center, textAlign) as CSS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = 
  { username :: String
  , password :: String
  , error :: Maybe String
  , isSubmitting :: Boolean
  }

data Action 
  = SetUsername String
  | SetPassword String
  | Login
  | NavigateToSignup

type Input = Unit
type Output = LoginOutput

data LoginOutput 
  = LoginSuccessful 
  | GoToSignup

component :: forall m query. MonadAff m => H.Component query Input Output m
component = 
  H.mkComponent
    { initialState: const { username: "", password: "", error: Nothing, isSubmitting: false }
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HCSS.style do
        CSS.display CSS.flex
        CSS.flexDirection CSS.column
        CSS.height (CSS.vh 80.0)
        CSS.width (CSS.pct 100.0)
    ]
    [ HH.div
        [ HCSS.style do
            CSS.width (CSS.px 350.0)
            CSS.padding (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0)
            CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 220 220 220)
            CSS.borderRadius (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
        ]
        [ HH.h2
            [ HCSS.style do
                CSS.textAlign CSS.center
                CSS.marginBottom (CSS.px 20.0)
            ]
            [ HH.text "Login" ]
        , renderErrorMessage state.error
        , formField "Username" "text" state.username SetUsername
        , formField "Password" "password" state.password SetPassword
        , HH.div
            [ HCSS.style do
                CSS.display CSS.flex
                CSS.flexDirection CSS.column
                CSS.marginTop (CSS.px 20.0)
            ]
            [ HH.button
                [ HP.disabled state.isSubmitting
                , HE.onClick \_ -> Login
                , HCSS.style do
                    CSS.width (CSS.pct 100.0)
                    CSS.padding (CSS.px 10.0) (CSS.px 0.0) (CSS.px 10.0) (CSS.px 0.0)
                    CSS.backgroundColor (CSS.rgb 33 150 243)
                    CSS.color (CSS.rgb 255 255 255)
                    CSS.border CSS.solid (CSS.px 0.0) (CSS.rgb 0 0 0)
                    CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                    CSS.fontSize (CSS.px 16.0)
                    CSS.cursor CSS.pointer
                ]
                [ HH.text $ if state.isSubmitting then "Logging in..." else "Login" ]
            , HH.div
                [ HCSS.style do
                    CSS.textAlign CSS.center
                    CSS.marginTop (CSS.px 15.0)
                    CSS.fontSize (CSS.px 14.0)
                ]
                [ HH.text "Don't have an account? "
                , HH.a
                    [ HE.onClick \_ -> NavigateToSignup
                    , HCSS.style do
                        CSS.color (CSS.rgb 33 150 243)
                        CSS.cursor CSS.pointer
                        CSS.textDecoration CSS.underline
                    ]
                    [ HH.text "Sign up" ]
                ]
            ]
        ]
    ]

renderErrorMessage :: forall action slots m. Maybe String -> H.ComponentHTML action slots m
renderErrorMessage = case _ of
  Nothing -> HH.text ""
  Just message -> 
    HH.div
      [ HCSS.style do
          CSS.padding (CSS.px 10.0) (CSS.px 15.0) (CSS.px 10.0) (CSS.px 15.0)
          CSS.marginBottom (CSS.px 15.0)
          CSS.backgroundColor (CSS.rgb 255 235 238)
          CSS.color (CSS.rgb 244 67 54)
          CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
      ]
      [ HH.text message ]

formField :: forall action slots m. String -> String -> String -> (String -> action) -> H.ComponentHTML action slots m
formField label type_ value onChange =
  HH.div
    [ HCSS.style do
        CSS.marginBottom (CSS.px 15.0)
    ]
    [ HH.label
        [ HCSS.style do
            CSS.display CSS.block
            CSS.marginBottom (CSS.px 5.0)
            CSS.fontSize (CSS.px 14.0)
        ]
        [ HH.text label ]
    , HH.input
        [ HP.type_ (fromTypeString type_)
        , HP.value value
        , HE.onValueInput onChange
        , HCSS.style do
            CSS.width (CSS.pct 100.0)
            CSS.padding (CSS.px 8.0) (CSS.px 10.0) (CSS.px 8.0) (CSS.px 10.0)
            CSS.border CSS.solid (CSS.px 1.0) (CSS.rgb 220 220 220)
            CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
            CSS.fontSize (CSS.px 16.0)
            CSS.boxSizing CSS.borderBox
        ]
    ]

fromTypeString :: String -> HP.InputType
fromTypeString = case _ of
  "text" -> HP.InputText
  "password" -> HP.InputPassword
  _ -> HP.InputText

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetUsername username -> 
    H.modify_ \st -> st { username = username }
  
  SetPassword password -> 
    H.modify_ \st -> st { password = password }
  
  Login -> do
    state <- H.get
    if state.username == "" || state.password == "" 
      then 
        H.modify_ \st -> st { error = Just "Username and password are required" }
      else do
        H.modify_ \st -> st { isSubmitting = true, error = Nothing }
        result <- H.liftAff $ Client.login $ UserCredentials 
          { username: state.username
          , email: Nothing
          , password: state.password 
          }
        case result of
          Right _ -> do
            H.raise LoginSuccessful
          Left err -> do
            H.modify_ \st -> st { error = Just $ "Login failed: " <> err, isSubmitting = false }

  NavigateToSignup -> 
    H.raise GoToSignup
