module Client.View.SVG where

import Prelude hiding (div)
import Common.Types (Board, PlayerId, PlayerState, Position(..))
import Data.Array as Array
import Data.Int (toNumber)
import Halogen.HTML (HTML)
import Halogen.Svg.Attributes (cx, cy, fill, height, id, r, viewBox, width)
import Halogen.Svg.Attributes.Color as Color
import Halogen.Svg.Elements (circle, g, svg)

renderBoard :: forall p i. { playerId :: PlayerId, board :: Board } -> HTML p i
renderBoard { playerId: myPlayerId, board } =
  svg
    [ width 800.0
    , height 150.0
    , viewBox 0.0 0.0 400.0 300.0
    ]
    [ g [ id "players" ] (viewPlayer myPlayerId <$> Array.fromFoldable board)
    ]

viewPlayer :: forall p i. PlayerId -> PlayerState -> HTML p i
viewPlayer myPlayerId { playerId, playerState: (Position { x, y }) } =
  g [ id "player" ]
    [ circle
        [ cx (toNumber x * 10.0)
        , cy (toNumber y * 10.0)
        , r 15.0
        , fill (Color.Named (if playerId == myPlayerId then "lightgreen" else "lightblue"))
        ]
    ]
