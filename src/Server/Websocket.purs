module Server.Websocket (main, shutdown, broadcast, State) where

import Prelude
import Common.Types (ServerMessage(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse_)
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Console (logShow, log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.TypeError (class Warn, Text)
import Simple.JSON (writeJSON)

type WebsocketServerConfig
  = { port :: Int
    }

foreign import data WebsocketServer :: Type

foreign import data WebsocketConnection :: Type

type WebsocketData
  = String

foreign import createServer :: WebsocketServerConfig -> Effect WebsocketServer

foreign import onConnection :: WebsocketServer -> (WebsocketConnection -> Effect Unit) -> Effect Unit

foreign import onMessage :: WebsocketConnection -> (WebsocketData -> Effect Unit) -> Effect Unit

foreign import send :: WebsocketConnection -> String -> Effect Unit

main ::
  Warn (Text "We aren't handling websocket disconnections at all.") =>
  WebsocketServerConfig -> Effect State
main config = do
  server <- createServer config
  connections <- Ref.new Map.empty
  let
    state = { connections }
  onConnection server
    $ \connection -> do
        newId <- genUUID
        Ref.modify_ (Map.insert newId connection) state.connections
        narrowcast connection (ServerMessage { statuses: [ { name: "HELLO" } ] })
        onMessage connection
          ( \msg -> do
              log $ "RECEVIED A MESSAGE: " <> msg
              narrowcast connection
                (ServerMessage { statuses: [ { name: msg } ] })
          )
  pure $ State state

shutdown :: Warn (Text "TODO Handle shutdown") => State -> Effect Unit
shutdown _state = pure unit

data State
  = State
    { connections :: Ref (Map UUID WebsocketConnection)
    }

-- | Send the state to everyone.
narrowcast :: WebsocketConnection -> ServerMessage -> Effect Unit
narrowcast connection msg = do
  send connection $ writeJSON msg

broadcast :: State -> ServerMessage -> Effect Unit
broadcast (State state) msg = do
  let
    json = writeJSON msg
  connections <- Ref.read state.connections
  traverse_ (flip send json) connections
