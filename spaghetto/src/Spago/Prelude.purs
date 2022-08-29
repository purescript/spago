module Spago.Prelude
  ( module Prelude
  , module Extra
  , Spago(..)
  , runSpago
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Reader (ask, asks) as Extra
import Data.Array ((..)) as Extra
import Data.DateTime.Instant (Instant) as Extra
import Data.Either (Either(..), isLeft, isRight) as Extra
import Data.Filterable (partition, partitionMap) as Extra
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
import Effect.Aff (Aff, Error) as Extra
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception.Unsafe (unsafeThrow) as Extra
import Effect.Ref (Ref) as Extra
import Spago.Log (log, logShow) as Extra

newtype Spago env a = Spago (ReaderT env Extra.Aff a)

derive instance Extra.Newtype (Spago env a) _

derive newtype instance Functor (Spago env)
derive newtype instance Apply (Spago env)
derive newtype instance Applicative (Spago env)
derive newtype instance Bind (Spago env)
derive newtype instance Monad (Spago env)
derive newtype instance MonadEffect (Spago env)
derive newtype instance MonadAff (Spago env)
derive newtype instance MonadThrow Extra.Error (Spago env)
derive newtype instance MonadAsk env (Spago env)

runSpago :: forall a env. env -> Spago env a -> Extra.Aff a
runSpago env (Spago m) = runReaderT m env
