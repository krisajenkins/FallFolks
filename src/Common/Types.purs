module Common.Types where

import Prelude
import Control.Monad.Except (throwError)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype PlayerId
  = PlayerId UUID

derive instance genericPlayerId :: Generic PlayerId _

derive newtype instance eqPlayerId :: Eq PlayerId

derive newtype instance ordPlayerId :: Ord PlayerId

instance showPlayerId :: Show PlayerId where
  show (PlayerId uuid) = "(PlayerId " <> UUID.toString uuid <> ")"

instance readForeignPlayerId :: ReadForeign PlayerId where
  readImpl x = do
    str :: String <- readImpl x
    case parseUUID str of
      Nothing -> throwError $ NonEmptyList $ NonEmpty (TypeMismatch "UUID" str) mempty
      Just v -> pure $ PlayerId v

instance writeForeignPlayerId :: WriteForeign PlayerId where
  writeImpl (PlayerId x) = writeImpl (UUID.toString x)

------------------------------------------------------------
newtype ClientMessage
  = SetName String

derive instance genericClientMessage :: Generic ClientMessage _

instance showClientMessage :: Show ClientMessage where
  show x = genericShow x

derive newtype instance readForeignClientMessage :: ReadForeign ClientMessage

derive newtype instance writeForeignClientMessage :: WriteForeign ClientMessage

------------------------------------------------------------
newtype ServerMessage
  = ServerMessage
  { board :: Board }

type Board
  = Array
      { playerId :: PlayerId
      , playerState :: String
      }

derive instance genericServerMessage :: Generic ServerMessage _

instance showServerMessage :: Show ServerMessage where
  show x = genericShow x

derive newtype instance readForeignServerMessage :: ReadForeign ServerMessage

derive newtype instance writeForeignServerMessage :: WriteForeign ServerMessage
