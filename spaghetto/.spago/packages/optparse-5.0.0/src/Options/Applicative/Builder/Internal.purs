module Options.Applicative.Builder.Internal (
  -- * Internals
  Mod(..),
  class HasName, name,
  class HasCompleter, modCompleter,
  class HasValue, hasValueDummy,
  class HasMetavar, hasMetavarDummy,
  OptionFields(..),
  FlagFields(..),
  CommandFields(..),
  ArgumentFields(..),
  DefaultProp(..),

  optionMod,
  fieldMod,

  baseProps,
  mkCommand,
  mkParser,
  mkOption,
  mkProps,

  internal
  ) where

import Prelude
import Options.Applicative.Common (liftOpt)
import Options.Applicative.Types (Completer, OptName, OptProperties(..), OptReader, OptVisibility(..), Option(..), ParseError, Parser, ParserInfo)
import Control.Alt (alt)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple, fst)
import Data.Foldable (lookup)
import Data.Tuple.Nested (Tuple3, (/\))

newtype OptionFields :: forall k. k -> Type
newtype OptionFields a = OptionFields
  { optNames :: Array OptName
  , optCompleter :: Completer
  , optNoArgError :: String -> ParseError }
derive instance newtypeOptionFields :: Newtype (OptionFields a) _

newtype FlagFields a = FlagFields
  { flagNames :: Array OptName
  , flagActive :: a }
derive instance newtypeFlagFields :: Newtype (FlagFields a) _

newtype CommandFields a = CommandFields
  { cmdCommands :: Array (Tuple String (ParserInfo a))
  , cmdGroup :: Maybe String }
derive instance newtypeCommandFields :: Newtype (CommandFields a) _

newtype ArgumentFields :: forall k. k -> Type
newtype ArgumentFields a = ArgumentFields
  { argCompleter :: Completer }
derive instance newtypeArgumentFields :: Newtype (ArgumentFields a) _

class HasName :: forall k. (k -> Type) -> Constraint
class HasName f where
  name :: forall a. OptName -> f a -> f a


instance optionFieldsHasName :: HasName OptionFields where
  name n = over OptionFields \fields -> fields{ optNames = [n] <> fields.optNames }

instance flagFieldsHasName :: HasName FlagFields where
  name n = over FlagFields \fields -> fields{ flagNames = [n] <> fields.flagNames }

class HasCompleter :: forall k. (k -> Type) -> Constraint
class HasCompleter f where
  modCompleter :: forall a. (Completer -> Completer) -> f a -> f a

instance optionFieldsHasCompleter :: HasCompleter OptionFields where
  modCompleter f = over OptionFields \p -> p{ optCompleter = f p.optCompleter }

instance argumentFieldsHasCompleter :: HasCompleter ArgumentFields where
  modCompleter f = over ArgumentFields \p -> p{ argCompleter = f p.argCompleter }

class HasValue :: forall k. (k -> Type) -> Constraint
class HasValue f where
  -- this is just so that it is not necessary to specify the kind of f
  hasValueDummy :: forall a. f a -> Unit
instance optionFieldsHasValue :: HasValue OptionFields where
  hasValueDummy _ = unit
instance argumentFieldsHasValue :: HasValue ArgumentFields where
  hasValueDummy _ = unit

class HasMetavar :: forall k. (k -> Type) -> Constraint
class HasMetavar f where
  hasMetavarDummy :: forall a. f a -> Unit
instance optionFieldsHasMetavar :: HasMetavar OptionFields where
  hasMetavarDummy _ = unit
instance argumentFieldsHasMetavar :: HasMetavar ArgumentFields where
  hasMetavarDummy _ = unit
instance commandFieldsHasMetavar :: HasMetavar CommandFields where
  hasMetavarDummy _ = unit

-- mod --

data DefaultProp a = DefaultProp
  (Maybe a)
  (Maybe (a -> String))

instance defaultPropMonoid :: Monoid (DefaultProp a) where
  mempty = DefaultProp Nothing Nothing

instance defaultPropSemigroup :: Semigroup (DefaultProp a) where
  append (DefaultProp d1 s1) (DefaultProp d2 s2) =
    DefaultProp (d1 `alt` d2) (s1 `alt` s2)

-- | An option modifier.
-- |
-- | Option modifiers are values that represent a modification of the properties
-- | of an option.
-- |
-- | The type parameter @a@ is the pure type of the option, while @f@ is a
-- | record containing its properties (e.g. 'OptionFields' for regular options,
-- | 'FlagFields' for flags, etc...).
-- |
-- | An option modifier consists of 3 elements:
-- |
-- |  - A field modifier, of the form @f a -> f a@. These are essentially
-- |  (compositions of) setters for some of the properties supported by @f@.
-- |
-- |  - An optional default value and function to display it.
-- |
-- |  - A property modifier, of the form @OptProperties -> OptProperties@. This
-- |  is just like the field modifier, but for properties applicable to any
-- |  option.
-- |
-- | Modifiers are instances of 'Monoid', and can be composed as such.
-- |
-- | One rarely needs to deal with modifiers directly, as most of the times it is
-- | sufficient to pass them to builders (such as 'strOption' or 'flag') to
-- | create options (see 'Options.Applicative.Builder').
-- |
-- | Contraints are often used to ensure that the modifiers can be sensibly applied.
-- | For example, positional arguments can't be specified by long or short names,
-- | so the 'HasName' constraint is used to ensure we have a flag or option.
data Mod f a = Mod (f a -> f a)
                   (DefaultProp a)
                   (OptProperties -> OptProperties)

optionMod :: forall f a. (OptProperties -> OptProperties) -> Mod f a
optionMod = Mod identity mempty

fieldMod :: forall f a. (f a -> f a) -> Mod f a
fieldMod f = Mod f mempty identity

instance modMonoid :: Monoid (Mod f a) where
  mempty = Mod identity mempty identity

-- | @since 0.13.0.0
instance modSemigroup :: Semigroup (Mod f a) where
  append (Mod f1 d1 g1) (Mod f2 d2 g2)
    = Mod (f2 <<< f1) (d2 <> d1) (g2 <<< g1)

-- | Base default properties.
baseProps :: OptProperties
baseProps = OptProperties
  { propMetaVar: ""
  , propVisibility: Visible
  , propHelp: mempty
  , propShowDefault: Nothing
  , propDescMod: Nothing
  }

mkCommand :: forall a. Mod CommandFields a -> Tuple3 (Maybe String) (Array String) (String -> Maybe (ParserInfo a))
mkCommand m = group /\ map fst cmds /\ (_ `lookup` cmds) /\ unit
  where
    Mod f _ _ = m
    CommandFields {cmdCommands:cmds, cmdGroup:group} = f (CommandFields {cmdCommands: [], cmdGroup: Nothing})

mkParser :: forall a. DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Parser a
mkParser d@(DefaultProp def _) g rdr =
  let
    o = liftOpt $ mkOption d g rdr
  in
    maybe o (\a -> o `alt` pure a) def

mkOption :: forall a. DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Option a
mkOption d g rdr = Option { optMain: rdr, optProps: mkProps d g }

mkProps :: forall a. DefaultProp a
        -> (OptProperties -> OptProperties)
        -> OptProperties
mkProps (DefaultProp def sdef) g = props
  where
    props = over OptProperties (\r -> r { propShowDefault = sdef <*> def }) (g baseProps)

-- | Hide this option from the help text
internal :: forall f a. Mod f a
internal = optionMod $ over OptProperties \p -> p { propVisibility = Internal }
