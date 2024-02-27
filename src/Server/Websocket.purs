module Server.Websocket where

import Prelude
import Effect (Effect)

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
