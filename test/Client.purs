module Test.Client (spec) where

import Prelude
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Client" do
    it "Should run" do
      pure unit
