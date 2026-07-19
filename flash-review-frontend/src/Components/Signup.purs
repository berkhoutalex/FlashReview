module Components.Signup where

import Prelude

import API.Client as Client
import API.Types (UserCredentials(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
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
    [ HP.class_ (HH.ClassName "card auth-card") ]
    [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Sign Up" ]
    , renderErrorMessage state.error
    , formField "Username" "text" state.username SetUsername
    , formField "Email" "email" state.email SetEmail
    , formField "Password" "password" state.password SetPassword
    , formField "Confirm Password" "password" state.confirmPassword SetConfirmPassword
    , HH.button
        [ HP.class_ (HH.ClassName "btn btn-primary btn-block")
        , HP.disabled state.isSubmitting
        , HE.onClick \_ -> Signup
        ]
        [ HH.text $ if state.isSubmitting then "Creating account..." else "Create Account" ]
    , HH.div
        [ HP.class_ (HH.ClassName "auth-footer") ]
        [ HH.text "Already have an account? "
        , HH.a
            [ HP.class_ (HH.ClassName "link")
            , HE.onClick \_ -> NavigateToLogin
            ]
            [ HH.text "Login" ]
        ]
    ]

renderErrorMessage :: forall action slots m. Maybe String -> H.ComponentHTML action slots m
renderErrorMessage = case _ of
  Nothing -> HH.text ""
  Just message ->
    HH.div
      [ HP.class_ (HH.ClassName "alert alert-error") ]
      [ HH.text message ]

formField :: forall action slots m. String -> String -> String -> (String -> action) -> H.ComponentHTML action slots m
formField label type_ value onChange =
  HH.div
    [ HP.class_ (HH.ClassName "field") ]
    [ HH.label [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
    , HH.input
        [ HP.class_ (HH.ClassName "input")
        , HP.type_ (fromTypeString type_)
        , HP.value value
        , HE.onValueInput onChange
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
            H.raise SignupSuccessful
          Left err -> do
            H.modify_ \st -> st { error = Just $ "Signup failed: " <> err, isSubmitting = false }

  NavigateToLogin ->
    H.raise GoToLogin
