module Websocket.Client where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (MultipleErrors)
import Halogen.Subscription (Emitter, Listener, notify, subscribe)
import Halogen.Subscription as Subscription
import Network.RemoteData (RemoteData)
import Network.RemoteData as RemoteData
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

type WebsocketClientConfig
  = { url :: String }

foreign import data WebsocketClient :: Type

foreign import makeWebsocketClient :: WebsocketClientConfig -> Effect WebsocketClient

foreign import onmessage :: WebsocketClient -> (String -> Effect Unit) -> Effect Unit

foreign import send :: WebsocketClient -> String -> Effect Unit

type WebsocketChannels from to
  = { fromServer :: Emitter (RemoteData MultipleErrors from)
    , toServer :: Listener to
    }

makeWebsocketChannels ::
  forall m from to.
  MonadEffect m =>
  ReadForeign from =>
  WriteForeign to =>
  m (WebsocketChannels from to)
makeWebsocketChannels = do
  { from, to } <-
    liftEffect
      $ do
          from <- Subscription.create
          to <- Subscription.create
          websocketClient <-
            makeWebsocketClient
              { url: "ws://localhost:8080"
              }
          onmessage websocketClient
            $ \msg -> do
                notify from.listener $ RemoteData.fromEither $ readJSON msg
          _subscriptionTo <-
            subscribe to.emitter
              ( \msg ->
                  send websocketClient $ writeJSON msg
              )
          pure { from, to }
  pure
    { fromServer: from.emitter
    , toServer: to.listener
    }
