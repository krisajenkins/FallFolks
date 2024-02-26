module Client.View (render) where

import Prelude hiding (div)
import Client.Types (Action(..), Direction(..), State(..))
import Common.Types (ServerMessage)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.HTML (HTML, button, div, h1, i, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..))

render :: forall m. State -> ComponentHTML Action () m
render (State state) =
  div []
    [ h1 [] [ text "Fall Folks" ]
    , div []
        ( ( \direction ->
              button
                [ classes [ ClassName "btn", ClassName "btn-primary" ]
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
    div [ classes [ ClassName "alert", ClassName "alert-danger" ] ]
      [ text (show errors) ]
  Success payloads -> f payloads

viewMessages :: forall p i. ServerMessage -> HTML p i
viewMessages message = div [] [ text $ show message ]
