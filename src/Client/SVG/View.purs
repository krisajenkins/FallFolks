module Client.View.SVG where

import Prelude hiding (div)
import Common.Types (Board)
import Halogen.HTML (HTML)
import Halogen.Svg.Attributes (cx, cy, fill, height, r, viewBox, width)
import Halogen.Svg.Attributes.Color as Color
import Halogen.Svg.Elements (circle, svg)

renderBoard :: forall p i. Board -> HTML p i
renderBoard _board =
  svg
    [ width 800.0
    , height 300.0
    , viewBox 0.0 0.0 400.0 300.0
    ]
    [ circle
        [ cx 110.0
        , cy 90.0
        , r 85.0
        , fill (Color.Named "lightgreen")
        ]
    , circle
        [ cx 100.0
        , cy 100.0
        , r 75.0
        , fill (Color.Named "aliceblue")
        ]
    ]
