module Server.Game where

import Prelude
import Common.Types (Board, PlayerId)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen

newtype GameState
  = GameState (Map PlayerId String)

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

process :: PlayerId -> String -> GameState -> GameState
process player message (GameState state) = GameState (Map.insert player message state)
