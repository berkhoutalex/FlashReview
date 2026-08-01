module Main where

import Prelude

import API.Storage as Storage
import Components.App (component)
import Data.Maybe (isJust)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  hasToken <- isJust <$> Storage.getToken
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component hasToken body
