module Test.Common.Types (spec) where

import Prelude
import Common.Types (ClientMessage(..), Direction(..), ServerMessage, Timestamp)
import Data.Either (Either(..))
import Simple.JSON (readJSON, writeJSON)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Common.Types" do
    describe "ClientMessage" do
      it "ClientMessage should encode as expected." do
        (writeJSON (SetName "john")) `shouldEqual` "{\"_tag\":\"SetName\",\"_args\":\"john\"}"
        (writeJSON (Move North)) `shouldEqual` "{\"_tag\":\"Move\",\"_args\":\"North\"}"
      it "ClientMessage roundtrip to/from JSON" do
        quickCheck
          $ \(value :: ClientMessage) ->
              Right value === readJSON (writeJSON value)
    describe "ServerMessage" do
      it "ServerMessage roundtrip to/from JSON" do
        quickCheck
          $ \(value :: ServerMessage) ->
              Right value === readJSON (writeJSON value)
    describe "Timestamp" do
      it "Timestamp roundtrip to/from JSON" do
        quickCheck
          $ \(value :: Timestamp) ->
              Right value === readJSON (writeJSON value)
