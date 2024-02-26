module Common.Types where

import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype ClientMessage
  = SetName String

derive instance genericClientMessage :: Generic ClientMessage _

instance showClientMessage :: Show ClientMessage where
  show x = genericShow x

derive newtype instance readForeignClientMessage :: ReadForeign ClientMessage

derive newtype instance writeForeignClientMessage :: WriteForeign ClientMessage

------------------------------------------------------------
newtype ServerMessage
  = ServerMessage { statuses :: Array { name :: String } }

derive instance genericServerMessage :: Generic ServerMessage _

instance showServerMessage :: Show ServerMessage where
  show x = genericShow x

derive newtype instance readForeignServerMessage :: ReadForeign ServerMessage

derive newtype instance writeForeignServerMessage :: WriteForeign ServerMessage
