module Server.Webserver
  ( Webserver
  , WebsocketConnection
  , ConnectionHandler
  , WebserverConfig
  , createWebserver
  , runWebserver
  , addStatic
  , addWebsocket
  ) where

import Prelude
import Effect (Effect)

foreign import data Webserver :: Type

foreign import createWebserver :: Effect Webserver

foreign import runWebserver :: WebserverConfig -> Webserver -> Effect Unit

type WebserverConfig
  = { port :: Int }

foreign import data WebsocketConnection :: Type

type ConnectionHandler
  = (WebsocketConnection -> Effect Unit)

foreign import addWebsocket :: String -> Webserver -> ConnectionHandler -> Effect Unit

foreign import addStatic :: String -> Webserver -> Effect Unit
