module Spago.Core.Prelude
  ( module Prelude
  , module Extra
  , Spago(..)
  , unsafeFromJust
  , runSpago
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try, catchError) as Extra
import Control.Monad.Reader (ask, asks) as Extra
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.State (StateT) as Extra
import Data.Array ((..)) as Extra
import Data.Bifunctor (bimap, rmap, lmap) as Extra
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError) as Extra
import Data.DateTime.Instant (Instant) as Extra
import Data.Either (Either(..), isLeft, isRight, either, hush) as Extra
import Data.Filterable (partition, partitionMap) as Extra
import Data.Foldable (foldMap, for_, foldl, and, or) as Extra
import Data.Generic.Rep (class Generic) as Extra
import Data.Identity (Identity(..)) as Extra
import Data.List (List, (:)) as Extra
import Data.Map (Map) as Extra
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe, maybe) as Extra
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap) as Extra
import Data.Set (Set) as Extra
import Data.Show.Generic (genericShow) as Extra
import Data.Traversable (for, traverse) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..), fst, snd) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect) as Extra
import Effect.Aff (Aff, Error) as Extra
import Effect.Aff.Class (class MonadAff, liftAff) as Extra
import Effect.Class (class MonadEffect, liftEffect) as Extra
import Effect.Exception.Unsafe (unsafeThrow) as Extra
import Effect.Ref (Ref) as Extra
import Node.Buffer (Buffer) as Extra
import Node.Encoding (Encoding(..)) as Extra
import Node.Path (FilePath) as Extra
import Partial.Unsafe (unsafeCrashWith)
import Registry.ManifestIndex (ManifestIndex) as Extra
import Registry.Types (PackageName, Version, Range, Location, License, Manifest(..), Metadata(..), Sha256) as Extra
import Spago.Json (printJson, parseJson) as Extra
import Spago.Log (logDebug, logError, logInfo, Docc, logSuccess, logWarn, die, die', toDoc, indent, indent2, output, LogEnv, LogOptions, OutputFormat(..)) as Extra
import Spago.Yaml (YamlDoc, printYaml, parseYaml) as Extra

newtype Spago env a = Spago (ReaderT env Extra.Aff a)

derive instance Extra.Newtype (Spago env a) _

derive newtype instance Functor (Spago env)
derive newtype instance Apply (Spago env)
derive newtype instance Applicative (Spago env)
derive newtype instance Bind (Spago env)
derive newtype instance Monad (Spago env)
derive newtype instance Extra.MonadEffect (Spago env)
derive newtype instance Extra.MonadAff (Spago env)
derive newtype instance Extra.MonadThrow Extra.Error (Spago env)
derive newtype instance Extra.MonadError Extra.Error (Spago env)
derive newtype instance MonadAsk env (Spago env)

runSpago' :: forall a env. env -> Spago env a -> Extra.Aff a
runSpago' env (Spago m) = runReaderT m env

runSpago :: forall m a env. Extra.MonadAff m => env -> Spago env a -> m a
runSpago env a = Extra.liftAff (runSpago' env a)

unsafeFromJust :: forall a. Extra.Maybe a -> a
unsafeFromJust = Maybe.fromMaybe' (\_ -> unsafeCrashWith $ "Unexpected Nothing")
