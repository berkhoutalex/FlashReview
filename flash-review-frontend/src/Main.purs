module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Components.App (component)
import Theme as Theme

main :: Effect Unit
main = HA.runHalogenAff do
  initialTheme <- liftEffect Theme.initTheme
  body <- HA.awaitBody
  runUI component initialTheme body
