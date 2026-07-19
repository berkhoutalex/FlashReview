module Theme
  ( initTheme
  , setTheme
  ) where

import Prelude

import Effect (Effect)

foreign import initThemeImpl :: Effect String
foreign import setThemeImpl :: String -> Effect Unit

initTheme :: Effect String
initTheme = initThemeImpl

setTheme :: String -> Effect Unit
setTheme = setThemeImpl
