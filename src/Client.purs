module Client where

import Prelude
import Client.Types (Action(..), State(..), _websocketChannels, _messages)
import Client.View (render)
import Client.Websocket (WebsocketChannels, makeWebsocketChannels)
import Common.Types (ClientMessage(..), ServerMessage)
import Data.Lens (assign, use)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription (notify)
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    websocketChannels <- makeWebsocketChannels
    body <- HA.awaitBody
    runUI (component websocketChannels) unit body

component ::
  forall q i o m.
  MonadEffect m =>
  (WebsocketChannels ServerMessage ClientMessage) -> Component q i o m
component websocketChannels = do
  mkComponent
    { initialState: const $ initialState websocketChannels
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: WebsocketChannels ServerMessage ClientMessage -> State
initialState websocketChannels =
  State
    { websocketClient: Nothing
    , messages: NotAsked
    , websocketChannels
    }

handleAction ::
  forall o m.
  MonadEffect m =>
  Action -> HalogenM State Action () o m Unit
handleAction Initialize = do
  websocketChannels <- use _websocketChannels
  _subscription <- H.subscribe $ MessageReceived <$> websocketChannels.fromServer
  pure unit

handleAction (MessageReceived msg) = do
  assign _messages msg

handleAction (MovePlayer direction) = do
  websocketChannels <- use _websocketChannels
  liftEffect $ log "Sending"
  liftEffect $ notify websocketChannels.toServer (SetName (show direction))
