module Server.Websocket (main, shutdown, broadcast, State) where

import Prelude
import Data.Tuple.Nested ((/\))
import Common.Types (PlayerId(..), ServerMessage(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse_)
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Console (log)
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
  lastMessage <- Ref.new Map.empty
  let
    state = { connections, lastMessage }
  onConnection server
    $ \connection -> do
        newId <- PlayerId <$> genUUID
        Ref.modify_ (Map.insert newId connection) state.connections
        narrowcastBoard connection (State state)
        onMessage connection
          ( \message -> do
              log $ "Recevied: " <> message
              Ref.modify_ (Map.insert newId message) state.lastMessage
              broadcastBoard (State state)
          )
  pure $ State state

shutdown :: Warn (Text "TODO Handle shutdown") => State -> Effect Unit
shutdown _state = pure unit

data State
  = State
    { connections :: Ref (Map PlayerId WebsocketConnection)
    , lastMessage :: Ref (Map PlayerId String)
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

narrowcastBoard ::
  WebsocketConnection ->
  State ->
  Effect Unit
narrowcastBoard connection state = do
  serverMessage <- getBoard state
  narrowcast connection serverMessage

broadcastBoard :: State -> Effect Unit
broadcastBoard (State state) = do
  serverMessage <- getBoard (State state)
  broadcast (State state) serverMessage

getBoard :: State -> Effect ServerMessage
getBoard (State state) = do
  currentMessages <- Ref.read state.lastMessage
  pure
    $ ( ServerMessage
          { statuses:
              currentMessages
                # Map.toUnfoldable
                # map
                    ( \(playerId /\ msg) -> { user: playerId, message: msg }
                    )
          }
      )
