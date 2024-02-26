module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Client as Test.Client
import Test.Server as Test.Server

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Test.Client.spec
        Test.Server.spec
