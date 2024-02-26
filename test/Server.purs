module Test.Server (spec) where

import Prelude
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Server" do
    it "Should run" do
      pure unit
