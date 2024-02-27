module Client.Types where

import Prelude
import Websocket.Client (WebsocketClient, WebsocketChannels)
import Common.Types (ClientMessage, ServerMessage)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign (MultipleErrors)
import Network.RemoteData (RemoteData)
import Type.Proxy (Proxy(..))

newtype State
  = State
  { websocketClient :: Maybe WebsocketClient
  , messages :: RemoteData MultipleErrors ServerMessage
  , websocketChannels :: WebsocketChannels ServerMessage ClientMessage
  }

derive instance newtypeState :: Newtype State _

_websocketClient :: Lens' State (Maybe WebsocketClient)
_websocketClient = _Newtype <<< prop (Proxy :: Proxy "websocketClient")

_websocketChannels :: Lens' State (WebsocketChannels ServerMessage ClientMessage)
_websocketChannels = _Newtype <<< prop (Proxy :: Proxy "websocketChannels")

_messages :: Lens' State (RemoteData MultipleErrors ServerMessage)
_messages = _Newtype <<< prop (Proxy :: Proxy "messages")

data Action
  = Initialize
  | MessageReceived (RemoteData MultipleErrors ServerMessage)
  | MovePlayer Direction

data Direction
  = North
  | South
  | West
  | East

instance showDirection :: Show Direction where
  show North = "North"
  show South = "South"
  show West = "West"
  show East = "East"
