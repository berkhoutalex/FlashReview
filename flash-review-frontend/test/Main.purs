module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.API.DateTimeSpec as DateTimeSpec
import Test.API.TypesSpec as TypesSpec
import Test.API.UUIDSpec as UUIDSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  DateTimeSpec.spec
  UUIDSpec.spec
  TypesSpec.spec
