module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Client as Test.Client
import Test.Common.Types as Test.Common.Types
import Test.Server as Test.Server
import Test.Server.Game as Test.Server.Game
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Simple.JSON.Generic as Test.Simple.JSON.Generic

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Test.Client.spec
        Test.Server.spec
        Test.Server.Game.spec
        Test.Common.Types.spec
        Test.Simple.JSON.Generic.spec
