module Simple.JSON.Generic where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Except (throwError)
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), Sum(..))
import Foreign (F, Foreign, ForeignError(..))
import Foreign.Index (readProp)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

class WriteForeignSumRep rep where
  writeSumRep :: rep -> Foreign

instance writeForeignSumRepGenericSum ::
  ( WriteForeignSumRep a
  , WriteForeignSumRep b
  ) =>
  WriteForeignSumRep (Sum a b) where
  writeSumRep (Inl a) = writeSumRep a
  writeSumRep (Inr b) = writeSumRep b

instance writeForeignSumRepGenericConstructorNoArgs ::
  IsSymbol name =>
  WriteForeignSumRep (Constructor name NoArguments) where
  writeSumRep (Constructor NoArguments) = writeImpl name
    where
    name = reflectSymbol (Proxy :: Proxy name)

instance writeForeignSumRepGenericConstructorArgument ::
  (IsSymbol name, WriteForeign a) =>
  WriteForeignSumRep (Constructor name (Argument a)) where
  writeSumRep (Constructor (Argument a)) =
    writeImpl
      { _tag: name
      , _args: writeImpl a
      }
    where
    name = reflectSymbol (Proxy :: Proxy name)

------------------------------------------------------------
class ReadForeignSumRep rep where
  readSumRep :: Foreign -> F rep

instance readForeignSumRepGenericSum ::
  ( ReadForeignSumRep a
  , ReadForeignSumRep b
  ) =>
  ReadForeignSumRep (Sum a b) where
  readSumRep x =
    (Inl <$> readSumRep x)
      <|> (Inr <$> readSumRep x)

instance readForeignSumRepGenericConstructorArgument ::
  (IsSymbol name, ReadForeign a) =>
  ReadForeignSumRep (Constructor name (Argument a)) where
  readSumRep x = do
    s <- readProp "_tag" x >>= readImpl
    args <- readProp "_args" x >>= readImpl
    if s == name then
      pure $ Constructor (Argument args)
    else
      throwError <<< pure <<< ForeignError
        $ "Enum string "
        <> s
        <> " did not match expected string "
        <> name
    where
    name = reflectSymbol (Proxy :: Proxy name)

instance readForeignSumRepGenericConstructorNoArgs ::
  IsSymbol name =>
  ReadForeignSumRep (Constructor name NoArguments) where
  readSumRep x = do
    s <- readImpl x
    if s == name then
      pure $ Constructor NoArguments
    else
      throwError <<< pure <<< ForeignError
        $ "Enum string "
        <> s
        <> " did not match expected string "
        <> name
    where
    name = reflectSymbol (Proxy :: Proxy name)
