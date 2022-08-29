module Spago.Prelude
  ( module Prelude
  , module Extra
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Data.Array ((..)) as Extra
import Data.DateTime.Instant (Instant) as Extra
import Data.Either (Either(..), isLeft, isRight) as Extra
import Data.Foldable (foldMap, for_) as Extra
import Data.Generic.Rep (class Generic) as Extra
import Data.Identity (Identity(..)) as Extra
import Data.List (List, (:)) as Extra
import Data.Map (Map) as Extra
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe, maybe) as Extra
import Data.Newtype (class Newtype, unwrap) as Extra
import Data.Show.Generic (genericShow) as Extra
import Data.Traversable (for) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..)) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect) as Extra
import Effect.Aff (Aff) as Extra
import Effect.Console (log, logShow) as Extra
import Effect.Exception.Unsafe (unsafeThrow) as Extra
import Effect.Ref (Ref) as Extra
