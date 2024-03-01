module Server.Game where

import Prelude
import Common.Arbitrary (arbitraryMap)
import Common.Types (Board, ClientMessage(..), Direction(..), PlayerId, Position(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Tuple.Nested ((/\))
import Prim.TypeError (class Warn, Text)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype GameState
  = GameState (Map PlayerId Position)

derive instance newtypeGameState :: Newtype GameState _

instance arbitraryGameState :: Arbitrary GameState where
  arbitrary = GameState <$> arbitraryMap

toBoard :: GameState -> Board
toBoard (GameState currentMessages) =
  currentMessages
    # Map.toUnfoldable
    # map
        ( \(playerId /\ playerState) ->
            { playerId
            , playerState
            }
        )

process ::
  Warn (Text "Not processing SetName properly") =>
  PlayerId -> ClientMessage -> GameState -> GameState
process player (Move direction) = over GameState (Map.update (Just <<< move direction) player)

process _player (SetName _name) = identity

move :: Direction -> Position -> Position
move North (Position { x, y }) = Position { x, y: y + 1 }

move South (Position { x, y }) = Position { x, y: y - 1 }

move West (Position { x, y }) = Position { x: x - 1, y }

move East (Position { x, y }) = Position { x: x + 1, y }

newPlayer :: PlayerId -> Position
newPlayer _ = Position zero
