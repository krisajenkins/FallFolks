module Server.Game where

import Prelude
import Common.Types (Board, ClientMessage(..), Direction(..), PlayerId, Position(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Prim.TypeError (class Warn, Text)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen

newtype GameState
  = GameState (Map PlayerId Position)

derive instance newtypeGameState :: Newtype GameState _

instance arbitraryGameState :: Arbitrary GameState where
  arbitrary =
    (Tuple <$> arbitrary <*> arbitrary)
      # Gen.arrayOf
      # map Map.fromFoldable
      # map GameState

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
process player (Move direction) (GameState state) = GameState (Map.update (Just <<< move direction) player state)

process _player (SetName _name) (GameState state) = (GameState state)

move :: Direction -> Position -> Position
move North (Position { x, y }) = Position { x, y: y + 1 }

move South (Position { x, y }) = Position { x, y: y - 1 }

move West (Position { x, y }) = Position { x: x - 1, y }

move East (Position { x, y }) = Position { x: x + 1, y }

newPlayer :: PlayerId -> Position
newPlayer _ = Position { x: 0, y: 0 }
