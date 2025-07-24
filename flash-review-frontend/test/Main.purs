module Test.Main where

import Prelude

import API.Client.Spec (clientSpec)
import API.Types.Spec (typesSpec)
import Components.Spec (componentsSpec)
import Effect (Effect)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "Flash Review Frontend Tests" do
    typesSpec
    clientSpec
    componentsSpec
