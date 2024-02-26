module Logging
  ( console
  , class MonadLog
  , log
  ) where

import Prelude

import Control.Logger (Logger(..))
import Control.Logger as Logging
import Data.JSDate as JSDate
import Data.JSDate as JSDdate
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

class
  Monad m <= MonadLog m where
  log :: String -> m Unit

instance monadLogEffect :: MonadLog Effect where
  log str = Logging.log console str
else instance monadLogM :: MonadEffect m => MonadLog m where
  log str = liftEffect $ log str

console ::
  forall m.
  MonadEffect m =>
  Logger m String
console =
  Logger
    $ \msg ->
        liftEffect
          $ do
              retrieved <- JSDdate.now >>= JSDate.toISOString
              Console.log $ retrieved <> " " <> msg
