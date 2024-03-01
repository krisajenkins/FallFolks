module Common.Arbitrary where

import Control.Applicative (map, (<$>))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Function ((#), ($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Generic
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (class Ord)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, class ArbitraryGenericSum, arbitrary, arbitraryGenericSum)
import Test.QuickCheck.Gen (Gen, oneOf)
import Test.QuickCheck.Gen as Gen

arbitraryMap :: forall k v. Arbitrary k => Arbitrary v => Ord k => Gen (Map k v)
arbitraryMap =
  arbitrary
    # Gen.arrayOf
    # map Map.fromFoldable

-- | This is safe provided the sum type has at least one element.
arbitrarySum ::
  forall a rep.
  Generic a rep =>
  ArbitraryGenericSum rep =>
  Gen a
arbitrarySum =
  oneOf
    $ unsafePartial
    $ fromJust
    $ NonEmptyArray.fromArray
    $ (map Generic.to <$> arbitraryGenericSum)
