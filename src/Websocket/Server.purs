module Websocket.Server where

import Prelude

import Effect (Effect)
import Server.Webserver (WebsocketConnection)

type WebsocketServerConfig
  = { port :: Int
    }

foreign import data WebsocketServer :: Type

foreign import onMessage :: WebsocketConnection -> (String -> Effect Unit) -> Effect Unit

foreign import send :: WebsocketConnection -> String -> Effect Unit
