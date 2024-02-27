module Server (main) where

import Prelude
import Common.Types (PlayerId(..), ServerMessage(..))
import Data.Map as Map
import Data.Traversable (traverse_)
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Logging (log)
import Prim.TypeError (class Warn, Text)
import Server.Game (GameState(..), toBoard)
import Server.Game as Game
import Server.Types (State(..))
import Websocket.Server (WebsocketConnection, WebsocketServerConfig, createServer, onConnection, onMessage, send)
import Simple.JSON (writeJSON)

main :: Effect Unit
main = do
  log "START"
  launchAff_
    $ bracket
        ( do
            webserver <- liftEffect $ startWebsocketServer { port: 8080 }
            pure webserver
        )
        ( \_webserer -> do
            log "Shutdown Hook"
        )
        ( \_webserver -> do
            log "TODO"
        )
  log "END"

------------------------------------------------------------
startWebsocketServer ::
  Warn (Text "We aren't handling websocket disconnections at all.") =>
  WebsocketServerConfig -> Effect State
startWebsocketServer config = do
  server <- createServer config
  connections <- Ref.new Map.empty
  gameState <- Ref.new (GameState Map.empty)
  let
    state = { connections, gameState }
  onConnection server
    $ \connection -> do
        newId <- PlayerId <$> genUUID
        Ref.modify_ (Map.insert newId connection) state.connections
        narrowcastBoard connection (State state)
        onMessage connection
          ( \message -> do
              log $ "Recevied: " <> message
              Ref.modify_ (Game.process newId message) state.gameState
              broadcastBoard (State state)
          )
  pure $ State state

shutdown :: Warn (Text "TODO Handle shutdown") => State -> Effect Unit
shutdown _state = pure unit

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
  currentMessages <- Ref.read state.gameState
  pure
    $ ServerMessage
        { board: toBoard currentMessages
        }
