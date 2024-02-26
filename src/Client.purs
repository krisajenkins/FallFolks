module Client where

import Prelude

import Client.Types (Action(..), State(..), _kws, _messages)
import Client.View (render)
import Client.Websocket (KWS, makeKWS)
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
    kws <- makeKWS
    body <- HA.awaitBody
    runUI (component kws) unit body

component :: forall q i o m. MonadEffect m => (KWS ServerMessage ClientMessage) -> Component q i o m
component kws = do
  mkComponent
    { initialState: const $ initialState kws
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: KWS ServerMessage ClientMessage -> State
initialState kws =
  State
    { websocketClient: Nothing
    , messages: NotAsked
    , kws
    }

handleAction ::
  forall o m.
  MonadEffect m =>
  Action -> HalogenM State Action () o m Unit
handleAction Initialize = do
  kws <- use _kws
  _subscription <- H.subscribe $ MessageReceived <$> kws.fromServer
  pure unit

handleAction (MessageReceived msg) = do
  assign _messages msg

handleAction (MovePlayer direction) = do
  kws <- use _kws
  liftEffect $ log "Sending"
  liftEffect $ notify kws.toServer (SetName (show direction))
  pure unit
