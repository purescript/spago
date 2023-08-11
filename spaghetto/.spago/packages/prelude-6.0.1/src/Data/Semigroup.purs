module Data.Semigroup
  ( class Semigroup
  , append
  , (<>)
  , class SemigroupRecord
  , appendRecord
  ) where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

-- | The `Semigroup` type class identifies an associative operation on a type.
-- |
-- | Instances are required to satisfy the following law:
-- |
-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
-- |
-- | One example of a `Semigroup` is `String`, with `(<>)` defined as string
-- | concatenation. Another example is `List a`, with `(<>)` defined as
-- | list concatenation.
-- |
-- | ### Newtypes for Semigroup
-- |
-- | There are two other ways to implement an instance for this type class
-- | regardless of which type is used. These instances can be used by
-- | wrapping the values in one of the two newtypes below:
-- | 1. `First` - Use the first argument every time: `append first _ = first`.
-- | 2. `Last` - Use the last argument every time: `append _ last = last`.
class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

instance semigroupString :: Semigroup String where
  append = concatString

instance semigroupUnit :: Semigroup Unit where
  append _ _ = unit

instance semigroupVoid :: Semigroup Void where
  append _ = absurd

instance semigroupFn :: Semigroup s' => Semigroup (s -> s') where
  append f g x = f x <> g x

instance semigroupArray :: Semigroup (Array a) where
  append = concatArray

instance semigroupProxy :: Semigroup (Proxy a) where
  append _ _ = Proxy

instance semigroupRecord :: (RL.RowToList row list, SemigroupRecord list row row) => Semigroup (Record row) where
  append = appendRecord (Proxy :: Proxy list)

foreign import concatString :: String -> String -> String
foreign import concatArray :: forall a. Array a -> Array a -> Array a

-- | A class for records where all fields have `Semigroup` instances, used to
-- | implement the `Semigroup` instance for records.
class SemigroupRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class SemigroupRecord rowlist row subrow | rowlist -> subrow where
  appendRecord :: Proxy rowlist -> Record row -> Record row -> Record subrow

instance semigroupRecordNil :: SemigroupRecord RL.Nil row () where
  appendRecord _ _ _ = {}

instance semigroupRecordCons ::
  ( IsSymbol key
  , Row.Cons key focus subrowTail subrow
  , SemigroupRecord rowlistTail row subrowTail
  , Semigroup focus
  ) =>
  SemigroupRecord (RL.Cons key focus rowlistTail) row subrow where
  appendRecord _ ra rb = insert (get ra <> get rb) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = appendRecord (Proxy :: Proxy rowlistTail) ra rb
