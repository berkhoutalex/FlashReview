module Components.Signup where

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
  , email :: String
  , password :: String
  , confirmPassword :: String
  , error :: Maybe String
  , isSubmitting :: Boolean
  }

data Action 
  = SetUsername String
  | SetEmail String
  | SetPassword String
  | SetConfirmPassword String
  | Signup
  | NavigateToLogin

type Input = Unit
type Output = SignupOutput

data SignupOutput 
  = SignupSuccessful 
  | GoToLogin

component :: forall m query. MonadAff m => H.Component query Input Output m
component = 
  H.mkComponent
    { initialState: const 
        { username: ""
        , email: ""
        , password: ""
        , confirmPassword: ""
        , error: Nothing
        , isSubmitting: false 
        }
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
            [ HH.text "Sign Up" ]
        , renderErrorMessage state.error
        , formField "Username" "text" state.username SetUsername
        , formField "Email" "email" state.email SetEmail
        , formField "Password" "password" state.password SetPassword
        , formField "Confirm Password" "password" state.confirmPassword SetConfirmPassword
        , HH.div
            [ HCSS.style do
                CSS.display CSS.flex
                CSS.flexDirection CSS.column
                CSS.marginTop (CSS.px 20.0)
            ]
            [ HH.button
                [ HP.disabled state.isSubmitting
                , HE.onClick \_ -> Signup
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
                [ HH.text $ if state.isSubmitting then "Creating account..." else "Create Account" ]
            , HH.div
                [ HCSS.style do
                    CSS.textAlign CSS.center
                    CSS.marginTop (CSS.px 15.0)
                    CSS.fontSize (CSS.px 14.0)
                ]
                [ HH.text "Already have an account? "
                , HH.a
                    [ HE.onClick \_ -> NavigateToLogin
                    , HCSS.style do
                        CSS.color (CSS.rgb 33 150 243)
                        CSS.cursor CSS.pointer
                        CSS.textDecoration CSS.underline
                    ]
                    [ HH.text "Login" ]
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
  "email" -> HP.InputEmail
  "password" -> HP.InputPassword
  _ -> HP.InputText

validateForm :: State -> Maybe String
validateForm state 
  | state.username == "" = Just "Username is required"
  | state.email == "" = Just "Email is required"
  | state.password == "" = Just "Password is required"
  | state.password /= state.confirmPassword = Just "Passwords don't match"
  | otherwise = Nothing

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetUsername username -> 
    H.modify_ \st -> st { username = username }
  
  SetEmail email -> 
    H.modify_ \st -> st { email = email }
  
  SetPassword password -> 
    H.modify_ \st -> st { password = password }
  
  SetConfirmPassword password -> 
    H.modify_ \st -> st { confirmPassword = password }
  
  Signup -> do
    state <- H.get
    case validateForm state of
      Just error ->
        H.modify_ \st -> st { error = Just error }
      Nothing -> do
        H.modify_ \st -> st { isSubmitting = true, error = Nothing }
        result <- H.liftAff $ Client.signup $ UserCredentials 
          { username: state.username
          , email: Just state.email
          , password: state.password 
          }
        case result of
          Right _ -> do
            -- After successful signup, redirect to login
            H.raise SignupSuccessful
          Left err -> do
            H.modify_ \st -> st { error = Just $ "Signup failed: " <> err, isSubmitting = false }
  
  NavigateToLogin ->
    H.raise GoToLogin
