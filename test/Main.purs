module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Client as Test.Client
import Test.Server as Test.Server
import Test.Server.Game as Test.Server.Game

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Test.Client.spec
        Test.Server.spec
        Test.Server.Game.spec
