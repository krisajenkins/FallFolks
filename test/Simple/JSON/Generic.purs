module Test.Simple.JSON.Generic (spec) where

import Prelude
import Common.Arbitrary (arbitrarySum)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Generic
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Simple.JSON.Generic (readSumRep, writeSumRep)
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Simple.JSON.Generic" do
    it "Should roundtrip" do
      quickCheck
        $ \(value :: Fruits) ->
            Right value === readJSON (writeJSON value)
    it "Fruits should encode as expected." do
      writeJSON (Apple 5) `shouldEqual` "{\"_tag\":\"Apple\",\"_args\":5}"
      writeJSON (Orange "test") `shouldEqual` "{\"_tag\":\"Orange\",\"_args\":\"test\"}"
      writeJSON (Banana false) `shouldEqual` "{\"_tag\":\"Banana\",\"_args\":false}"

------------------------------------------------------------
data Fruits
  = Apple Int
  | Orange String
  | Banana Boolean

derive instance eqFruits :: Eq Fruits

derive instance genericFruits :: Generic Fruits _

instance showFruits :: Show Fruits where
  show = genericShow

instance arbitraryFruits :: Arbitrary Fruits where
  arbitrary = arbitrarySum

instance writeForeignFruits :: WriteForeign Fruits where
  writeImpl x = writeSumRep $ Generic.from x

instance readForeignFruits :: ReadForeign Fruits where
  readImpl x = Generic.to <$> readSumRep x
