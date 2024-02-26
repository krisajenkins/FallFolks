module Client.Types where

import Prelude
import Client.Websocket (WebsocketClient, KWS)
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
  , kws :: KWS ServerMessage ClientMessage
  }

derive instance newtypeState :: Newtype State _

_websocketClient :: Lens' State (Maybe WebsocketClient)
_websocketClient = _Newtype <<< prop (Proxy :: Proxy "websocketClient")

_kws :: Lens' State (KWS ServerMessage ClientMessage)
_kws = _Newtype <<< prop (Proxy :: Proxy "kws")

_messages :: Lens' State (RemoteData MultipleErrors ServerMessage)
_messages = _Newtype <<< prop (Proxy :: Proxy "messages")

-- _socket :: Lens' State (Listener ClientMessage)
-- _socket = _Newtype <<< prop (Proxy :: Proxy "socket")
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
