module Server.Types where

import Common.Types (PlayerId)
import Data.Map (Map)
import Effect.Ref (Ref)
import Server.Game (GameState)
import Server.Websocket (WebsocketConnection)

data State
  = State
    { connections :: Ref (Map PlayerId WebsocketConnection)
    , gameState :: Ref GameState
    }
