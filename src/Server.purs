module Server (main) where

import Prelude

import Common.Types (ClientMessage, PlayerId(..), ServerMessage(..), Board)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Newtype (over)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign (MultipleErrors)
import Logging (log)
import Prim.TypeError (class Warn, Text)
import Server.Game (GameState(..), toBoard)
import Server.Game as Game
import Server.Types (State(..))
import Server.Webserver (Webserver, WebsocketConnection)
import Server.Webserver as Server.Webserver
import Simple.JSON (readJSON, writeJSON)
import Websocket.Server (onMessage, send)

main :: Effect Unit
main = do
  log "START"
  launchAff_
    $ bracket
        (
            liftEffect
                $ do
                    server <- Server.Webserver.createWebserver
                    installWebsocket server
                    Server.Webserver.addStatic "dist" server
                    Server.Webserver.runWebserver { port: 8080 } server
        )
        ( \_webserer -> do
            log "Shutdown Hook"
        )
        ( \_webserver -> do
            log "TODO"
        )
  log "END"

------------------------------------------------------------
installWebsocket ::
  Warn (Text "We aren't handling websocket disconnections at all.") =>
  Webserver -> Effect Unit
installWebsocket server = do
  connections <- Ref.new Map.empty
  gameState <- Ref.new (GameState Map.empty)
  let
    state = { connections, gameState }
  Server.Webserver.addWebsocket "/ws" server
    $ \connection -> do
        playerId <- PlayerId <$> genUUID
        Ref.modify_ (Map.insert playerId connection) state.connections
        Ref.modify_
          ( over GameState
              (Map.insert playerId (Game.newPlayer playerId))
          )
          state.gameState
        narrowcastBoard (playerId /\ connection) (State state)
        onMessage connection
          ( \rawMessage -> do
              log $ "Recevied: " <> rawMessage
              let
                message :: Either MultipleErrors ClientMessage
                message = readJSON rawMessage
              log $ "Decoded: " <> show message
              case message of
                Left err -> log $ "ERROR: " <> show err
                Right clientMessage -> do
                  Ref.modify_ (Game.process playerId clientMessage) state.gameState
                  broadcastBoard (State state)
          )

-- | Send the state to everyone.
narrowcast :: WebsocketConnection -> ServerMessage -> Effect Unit
narrowcast connection msg = do
  send connection $ writeJSON msg

broadcast :: State -> Board -> Effect Unit
broadcast (State state) board = do
  connections <- Ref.read state.connections
  void
    $ traverseWithIndex
        ( \playerId connection ->
            send connection $ writeJSON $ ServerMessage { playerId, board }
        )
        connections

narrowcastBoard ::
  (PlayerId /\ WebsocketConnection) ->
  State ->
  Effect Unit
narrowcastBoard (playerId /\ connection) state = do
  board <- getBoard state
  narrowcast connection $ ServerMessage { board, playerId }

broadcastBoard :: State -> Effect Unit
broadcastBoard (State state) = do
  log "Broadcasting board"
  board <- getBoard (State state)
  broadcast (State state) board

getBoard :: State -> Effect Board
getBoard (State state) = do
  currentMessages <- Ref.read state.gameState
  pure $ toBoard currentMessages
