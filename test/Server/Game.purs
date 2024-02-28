module Test.Server.Game (spec) where

import Prelude
import Common.Types (oppositeDirection)
import Data.Array (foldr)
import Data.Map as Map
import Data.Newtype (unwrap)
import Server.Game (move)
import Server.Game as Game
import Test.QuickCheck ((===), (>=?))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Server.Game" do
    describe "process" do
      it "Processing a message should never decrease the number of registered players." do
        quickCheck
          $ \playerId msg before ->
              let
                after = Game.process playerId msg before
              in
                Map.size (unwrap after) >=? Map.size (unwrap before)
    describe "move" do
      it "Moves + Opposite moves == original position." do
        quickCheck
          $ \position directions ->
              foldr move
                position
                (directions <> map oppositeDirection directions)
                === position
