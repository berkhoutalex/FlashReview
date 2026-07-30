-- | Persists the session JWT in localStorage so a page refresh does not log
-- | the user out. Readable by JS, which is the accepted tradeoff for a SPA on
-- | static hosting.
module API.Storage
  ( getToken
  , setToken
  , clearToken
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import setTokenImpl :: String -> Effect Unit
foreign import getTokenImpl :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)
foreign import clearTokenImpl :: Effect Unit

setToken :: String -> Effect Unit
setToken = setTokenImpl

getToken :: Effect (Maybe String)
getToken = getTokenImpl Nothing Just

clearToken :: Effect Unit
clearToken = clearTokenImpl
