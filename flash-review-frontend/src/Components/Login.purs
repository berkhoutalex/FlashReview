module Components.Login where

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
    [ HP.class_ (HH.ClassName "card auth-card") ]
    [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Login" ]
    , renderErrorMessage state.error
    , formField "Username" "text" state.username SetUsername
    , formField "Password" "password" state.password SetPassword
    , HH.button
        [ HP.class_ (HH.ClassName "btn btn-primary btn-block")
        , HP.disabled state.isSubmitting
        , HE.onClick \_ -> Login
        ]
        [ HH.text $ if state.isSubmitting then "Logging in..." else "Login" ]
    , HH.div
        [ HP.class_ (HH.ClassName "auth-footer") ]
        [ HH.text "Don't have an account? "
        , HH.a
            [ HP.class_ (HH.ClassName "link")
            , HE.onClick \_ -> NavigateToSignup
            ]
            [ HH.text "Sign up" ]
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
