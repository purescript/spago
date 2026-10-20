module Lib (printNothing, module Pinned) where

import Prelude

import Control.Alt (class Alt) as Pinned
import Data.Functor.Invariant (class Invariant) as Pinned
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype) as Pinned
import Effect (Effect)
import Effect.Console (logShow)
import Safe.Coerce (coerce) as Pinned
import Unsafe.Coerce (unsafeCoerce) as Pinned

printNothing :: Effect Unit
printNothing = logShow $ isNothing Nothing
