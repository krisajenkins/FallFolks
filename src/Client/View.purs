module Client.View (render) where

import Bootstrap
import Client.Types (Action(..), State(..))
import Client.View.SVG (renderBoard)
import Common.Types (Direction(..), PlayerId, PlayerState, Position(..), ServerMessage(..))
import Data.Foldable (fold)
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, button, div, h1, h2, i, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..))
import Prelude (class Show, const, show, ($), (<$>), (==))

render :: forall m. State -> ComponentHTML Action () m
render (State state) =
  div []
    [ h1 [] [ text "Fall Folks" ]
    , div [ classes [ btnGroup ] ]
        ( ( \direction ->
              button
                [ classes [ btn, btnPrimary ]
                , onClick (const (MovePlayer direction))
                ]
                [ text $ show direction ]
          )
            <$> [ North, South, West, East ]
        )
    , viewRemoteData viewMessages state.messages
    ]

viewRemoteData :: forall p i e a. Show e => (a -> HTML p i) -> RemoteData e a -> HTML p i
viewRemoteData f value = case value of
  NotAsked -> div [] [ i [] [ text "Loading..." ] ]
  Loading -> div [] [ i [] [ text "Loading..." ] ]
  Failure errors ->
    div [ classes [ alert, alertDanger ] ]
      [ text (show errors) ]
  Success payloads -> f payloads

viewMessages :: forall p i. ServerMessage -> HTML p i
viewMessages (ServerMessage gameState) =
  div []
    [ h2 [] [ text "Board" ]
    , renderBoard gameState
    , div [] (viewPlayer gameState.playerId <$> gameState.board)
    ]

viewPlayer :: forall p i. PlayerId -> PlayerState -> HTML p i
viewPlayer myPlayerId { playerId, playerState: Position { x, y } } =
  div []
    [ text summary ]
  where
  summary =
    fold
      [ show playerId
      , ": "
      , show x
      , " "
      , show y
      , " "
      , if playerId == myPlayerId then "(Me)" else ""
      ]
