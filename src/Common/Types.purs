module Common.Types where

import Prelude
import Control.Monad.Except (throwError)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Data.Array (catMaybes)
import Data.Array.NonEmpty as NonEmpty
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype PlayerId
  = PlayerId UUID

derive instance genericPlayerId :: Generic PlayerId _

derive newtype instance eqPlayerId :: Eq PlayerId

derive newtype instance ordPlayerId :: Ord PlayerId

instance arbitraryPlayerId :: Arbitrary PlayerId where
  arbitrary = PlayerId <$> arbitraryUUID

arbitraryUUID :: forall m. MonadGen m => m UUID
arbitraryUUID =
  [ "0805fe16-d59a-11ee-84ac-325096b39f47"
  , "08060078-d59a-11ee-aac8-325096b39f47"
  , "080600fa-d59a-11ee-b905-325096b39f47"
  , "08060172-d59a-11ee-ab7c-325096b39f47"
  , "080601ea-d59a-11ee-92d1-325096b39f47"
  , "08060244-d59a-11ee-b46e-325096b39f47"
  , "08060302-d59a-11ee-b741-325096b39f47"
  , "08060370-d59a-11ee-bf21-325096b39f47"
  , "080603d4-d59a-11ee-b842-325096b39f47"
  , "0806042e-d59a-11ee-86c0-325096b39f47"
  ]
    # map UUID.parseUUID
    # catMaybes
    # NonEmpty.cons' UUID.emptyUUID
    # Gen.elements

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
