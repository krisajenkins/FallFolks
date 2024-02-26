module Server (main) where

import Prelude
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Logging (log)
import Halogen.Subscription as Subscription
import Server.Websocket as Server.Websocket

main :: Effect Unit
main = do
  log "START"
  { emitter, listener } <- Subscription.create
  launchAff_
    $ bracket
        ( do
            webserver <- liftEffect $ Server.Websocket.main { port: 8080 }
            pure webserver
        )
        ( \webserer -> do
            log "Shutdown Hook"
        )
        ( \webserver -> do
            log "TODO"
        )
  log "END"
