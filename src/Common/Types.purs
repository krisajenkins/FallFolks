module Common.Types where

import Prelude
import Common.Arbitrary (arbitrarySum)
import Control.Monad.Except (except, throwError)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Data.Array (catMaybes)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Gen (genDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformatParser)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Generic
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError(..), F)
import Parsing (ParserT, parseErrorMessage, runParser)
import Parsing as P
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Simple.JSON.Generic (readSumRep, writeSumRep)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

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
    # NonEmptyArray.cons' UUID.emptyUUID
    # Gen.elements

instance showPlayerId :: Show PlayerId where
  show (PlayerId uuid) = "(PlayerId " <> UUID.toString uuid <> ")"

instance readForeignPlayerId :: ReadForeign PlayerId where
  readImpl o = do
    str :: String <- readImpl o
    case parseUUID str of
      Nothing -> throwError $ NonEmptyList $ NonEmpty (TypeMismatch "UUID" str) mempty
      Just v -> pure $ PlayerId v

instance writeForeignPlayerId :: WriteForeign PlayerId where
  writeImpl (PlayerId x) = writeImpl (UUID.toString x)

------------------------------------------------------------
data ClientMessage
  = SetName String
  | Move Direction

derive instance eqClientMessage :: Eq ClientMessage

derive instance genericClientMessage :: Generic ClientMessage _

instance arbitraryClientMessage :: Arbitrary ClientMessage where
  arbitrary = genericArbitrary

instance showClientMessage :: Show ClientMessage where
  show x = genericShow x

instance writeForeignClientMessage :: WriteForeign ClientMessage where
  writeImpl x = writeSumRep $ Generic.from x

instance readForeignClientMessage :: ReadForeign ClientMessage where
  readImpl x = Generic.to <$> readSumRep x

------------------------------------------------------------
type PlayerState
  = { playerId :: PlayerId
    , playerState :: Position
    }

type Board
  = Array PlayerState

------------------------------------------------------------
newtype ServerMessage
  = ServerMessage
  { board :: Board
  , playerId :: PlayerId
  }

derive newtype instance eqServerMessage :: Eq ServerMessage

derive instance genericServerMessage :: Generic ServerMessage _

instance showServerMessage :: Show ServerMessage where
  show x = genericShow x

derive newtype instance arbitraryServerMessage :: Arbitrary ServerMessage

derive newtype instance readForeignServerMessage :: ReadForeign ServerMessage

derive newtype instance writeForeignServerMessage :: WriteForeign ServerMessage

------------------------------------------------------------
newtype Position
  = Position { x :: Int, y :: Int }

derive instance eqPosition :: Eq Position

derive instance ordPosition :: Ord Position

derive newtype instance semiRingPosition :: Semiring Position

derive newtype instance ringPosition :: Ring Position

derive instance newtypePosition :: Newtype Position _

derive instance genericPosition :: Generic Position _

instance showPosition :: Show Position where
  show x = genericShow x

derive newtype instance readForeignPosition :: ReadForeign Position

derive newtype instance arbitraryPosition :: Arbitrary Position

derive newtype instance writeForeignPosition :: WriteForeign Position

------------------------------------------------------------
data Direction
  = North
  | South
  | West
  | East

derive instance eqDirection :: Eq Direction

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show x = genericShow x

instance arbitraryDirection :: Arbitrary Direction where
  arbitrary = arbitrarySum

instance writeForeignDirection :: WriteForeign Direction where
  writeImpl x = writeSumRep (Generic.from x)

instance readForeignDirection :: ReadForeign Direction where
  readImpl x = Generic.to <$> readSumRep x

oppositeDirection :: Direction -> Direction
oppositeDirection North = South

oppositeDirection South = North

oppositeDirection West = East

oppositeDirection East = West

------------------------------------------------------------
newtype Timestamp
  = Timestamp DateTime

derive newtype instance eqTimestamp :: Eq Timestamp

derive instance genericTimestamp :: Generic Timestamp _

instance showTimestamp :: Show Timestamp where
  show = genericShow

instance readForeignTimestamp :: ReadForeign Timestamp where
  readImpl o = do
    str <- readImpl o
    Timestamp <$> readDateTime str

instance writeForeignTimestamp :: WriteForeign Timestamp where
  writeImpl (Timestamp x) = writeImpl $ format extendedDateTimeFormatInUTC x

instance arbitraryTimestamp :: Arbitrary Timestamp where
  arbitrary = Timestamp <$> genDateTime

parseDateTime :: forall m. Monad m => ParserT String m DateTime
parseDateTime = unformatParser extendedDateTimeFormatInUTC

extendedDateTimeFormatInUTC :: Formatter
extendedDateTimeFormatInUTC =
  parseFormatString "YYYY-MM-DDTHH:mm:ss.SSSZ"
    # either unsafeCrashWith identity

readDateTime :: String -> F DateTime
readDateTime str = except $ lmap toForeignError $ runParser str parseDateTime
  where
  toForeignError = NonEmptyList.singleton <<< TypeMismatch "DateTime" <<< parseErrorMessage
