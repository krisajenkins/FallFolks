module Test.Server.Game (spec) where

import Prelude
import Common.Types (Position, oppositeDirection)
import Data.Array (foldr)
import Data.Array as Array
import Data.Map as Map
import Data.Newtype (unwrap)
import Server.Game (move)
import Server.Game as Game
import Test.QuickCheck ((/==), (<=?), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Server.Game" do
    describe "process" do
      it "Processing a message should never change the number of registered players." do
        quickCheck
          $ \playerId msg before ->
              let
                after = Game.process playerId msg before
              in
                Map.size (unwrap after) === Map.size (unwrap before)
    describe "move" do
      it "A single move should always leave you in a different place." do
        quickCheck
          $ \position direction ->
              move direction position /== position
      it "Moves + Opposite moves == original position." do
        quickCheck
          $ \position directions ->
              foldr move
                position
                (directions <> map oppositeDirection directions)
                === position
      it "You can't travel further (Manhattan distance) than the total number of moves." do
        quickCheck
          $ \directions ->
              let
                endPosition :: Position
                endPosition = foldr move zero directions

                delta = unwrap $ abs endPosition
              in
                delta.x + delta.y <=? Array.length directions

abs :: forall a. Ord a => Ring a => a -> a
abs x
  | x >= zero = x
  | otherwise = negate x
