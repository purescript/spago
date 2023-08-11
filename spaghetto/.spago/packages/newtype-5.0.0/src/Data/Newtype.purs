module Data.Newtype where

import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Safe.Coerce (class Coercible, coerce)

-- | A type class for `newtype`s to enable convenient wrapping and unwrapping,
-- | and the use of the other functions in this module.
-- |
-- | The compiler can derive instances of `Newtype` automatically:
-- |
-- | ``` purescript
-- | newtype EmailAddress = EmailAddress String
-- |
-- | derive instance newtypeEmailAddress :: Newtype EmailAddress _
-- | ```
-- |
-- | Note that deriving for `Newtype` instances requires that the type be
-- | defined as `newtype` rather than `data` declaration (even if the `data`
-- | structurally fits the rules of a `newtype`), and the use of a wildcard for
-- | the wrapped type.
class Newtype :: Type -> Type -> Constraint
class Coercible t a <= Newtype t a | t -> a

wrap :: forall t a. Newtype t a => a -> t
wrap = coerce

unwrap :: forall t a. Newtype t a => t -> a
unwrap = coerce

instance newtypeAdditive :: Newtype (Additive a) a

instance newtypeMultiplicative :: Newtype (Multiplicative a) a

instance newtypeConj :: Newtype (Conj a) a

instance newtypeDisj :: Newtype (Disj a) a

instance newtypeDual :: Newtype (Dual a) a

instance newtypeEndo :: Newtype (Endo c a) (c a a)

instance newtypeFirst :: Newtype (First a) a

instance newtypeLast :: Newtype (Last a) a

-- | Given a constructor for a `Newtype`, this returns the appropriate `unwrap`
-- | function.
un :: forall t a. Newtype t a => (a -> t) -> t -> a
un _ = unwrap

-- | This combinator unwraps the newtype, applies a monomorphic function to the 
-- | contained value and wraps the result back in the newtype
modify :: forall t a. Newtype t a => (a -> a) -> t -> t
modify fn t = wrap (fn (unwrap t))

-- | This combinator is for when you have a higher order function that you want
-- | to use in the context of some newtype - `foldMap` being a common example:
-- |
-- | ``` purescript
-- | ala Additive foldMap [1,2,3,4] -- 10
-- | ala Multiplicative foldMap [1,2,3,4] -- 24
-- | ala Conj foldMap [true, false] -- false
-- | ala Disj foldMap [true, false] -- true
-- | ```
ala
  :: forall f t a s b
   . Coercible (f t) (f a)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> ((b -> s) -> f t)
  -> f a
ala _ f = coerce (f wrap)

-- | Similar to `ala` but useful for cases where you want to use an additional
-- | projection with the higher order function:
-- |
-- | ``` purescript
-- | alaF Additive foldMap String.length ["hello", "world"] -- 10
-- | alaF Multiplicative foldMap Math.abs [1.0, -2.0, 3.0, -4.0] -- 24.0
-- | ```
-- |
-- | The type admits other possibilities due to the polymorphic `Functor`
-- | constraints, but the case described above works because ((->) a) is a
-- | `Functor`.
alaF
  :: forall f g t a s b
   . Coercible (f t) (f a)
  => Coercible (g s) (g b)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> (f t -> g s)
  -> f a
  -> g b
alaF _ = coerce

-- | Lifts a function operate over newtypes. This can be used to lift a
-- | function to manipulate the contents of a single newtype, somewhat like
-- | `map` does for a `Functor`:
-- |
-- | ``` purescript
-- | newtype Label = Label String
-- | derive instance newtypeLabel :: Newtype Label _
-- |
-- | toUpperLabel :: Label -> Label
-- | toUpperLabel = over Label String.toUpper
-- | ```
-- |
-- | But the result newtype is polymorphic, meaning the result can be returned
-- | as an alternative newtype:
-- |
-- | ``` purescript
-- | newtype UppercaseLabel = UppercaseLabel String
-- | derive instance newtypeUppercaseLabel :: Newtype UppercaseLabel _
-- |
-- | toUpperLabel' :: Label -> UppercaseLabel
-- | toUpperLabel' = over Label String.toUpper
-- | ```
over
  :: forall t a s b
   . Newtype t a
  => Newtype s b
  => (a -> t)
  -> (a -> b)
  -> t
  -> s
over _ = coerce

-- | Much like `over`, but where the lifted function operates on values in a
-- | `Functor`:
-- |
-- | ``` purescript
-- | findLabel :: String -> Array Label -> Maybe Label
-- | findLabel s = overF Label (Foldable.find (_ == s))
-- | ```
-- |
-- | The above example also demonstrates that the functor type is polymorphic
-- | here too, the input is an `Array` but the result is a `Maybe`.
overF
  :: forall f g t a s b
   . Coercible (f a) (f t)
  => Coercible (g b) (g s)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> (f a -> g b)
  -> f t
  -> g s
overF _ = coerce

-- | The opposite of `over`: lowers a function that operates on `Newtype`d
-- | values to operate on the wrapped value instead.
-- |
-- | ``` purescript
-- | newtype Degrees = Degrees Number
-- | derive instance newtypeDegrees :: Newtype Degrees _
-- |
-- | newtype NormalDegrees = NormalDegrees Number
-- | derive instance newtypeNormalDegrees :: Newtype NormalDegrees _
-- |
-- | normaliseDegrees :: Degrees -> NormalDegrees
-- | normaliseDegrees (Degrees deg) = NormalDegrees (deg % 360.0)
-- |
-- | asNormalDegrees :: Number -> Number
-- | asNormalDegrees = under Degrees normaliseDegrees
-- | ```
-- |
-- | As with `over` the `Newtype` is polymorphic, as illustrated in the example
-- | above - both `Degrees` and `NormalDegrees` are instances of `Newtype`,
-- | so even though `normaliseDegrees` changes the result type we can still put
-- | a `Number` in and get a `Number` out via `under`.
under
  :: forall t a s b
   . Newtype t a
  => Newtype s b
  => (a -> t)
  -> (t -> s)
  -> a
  -> b
under _ = coerce

-- | Much like `under`, but where the lifted function operates on values in a
-- | `Functor`:
-- |
-- | ``` purescript
-- | newtype EmailAddress = EmailAddress String
-- | derive instance newtypeEmailAddress :: Newtype EmailAddress _
-- |
-- | isValid :: EmailAddress -> Boolean
-- | isValid x = false -- imagine a slightly less strict predicate here
-- |
-- | findValidEmailString :: Array String -> Maybe String
-- | findValidEmailString = underF EmailAddress (Foldable.find isValid)
-- | ```
-- |
-- | The above example also demonstrates that the functor type is polymorphic
-- | here too, the input is an `Array` but the result is a `Maybe`.
underF
  :: forall f g t a s b
   . Coercible (f t) (f a)
  => Coercible (g s) (g b)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> (f t -> g s)
  -> f a
  -> g b
underF _ = coerce

-- | Lifts a binary function to operate over newtypes.
-- |
-- | ``` purescript
-- | newtype Meter = Meter Int
-- | derive newtype instance newtypeMeter :: Newtype Meter _
-- | newtype SquareMeter = SquareMeter Int
-- | derive newtype instance newtypeSquareMeter :: Newtype SquareMeter _
-- |
-- | area :: Meter -> Meter -> SquareMeter
-- | area = over2 Meter (*)
-- | ```
-- |
-- | The above example also demonstrates that the return type is polymorphic
-- | here too.
over2
  :: forall t a s b
   . Newtype t a
  => Newtype s b
  => (a -> t)
  -> (a -> a -> b)
  -> t
  -> t
  -> s
over2 _ = coerce

-- | Much like `over2`, but where the lifted binary function operates on
-- | values in a `Functor`.
overF2
  :: forall f g t a s b
   . Coercible (f a) (f t)
  => Coercible (g b) (g s)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> (f a -> f a -> g b)
  -> f t
  -> f t
  -> g s
overF2 _ = coerce

-- | The opposite of `over2`: lowers a binary function that operates on `Newtype`d
-- | values to operate on the wrapped value instead.
under2
  :: forall t a s b
   . Newtype t a
  => Newtype s b
  => (a -> t)
  -> (t -> t -> s)
  -> a
  -> a
  -> b
under2 _ = coerce

-- | Much like `under2`, but where the lifted binary function operates on
-- | values in a `Functor`.
underF2
  :: forall f g t a s b
   . Coercible (f t) (f a)
  => Coercible (g s) (g b)
  => Newtype t a
  => Newtype s b
  => (a -> t)
  -> (f t -> f t -> g s)
  -> f a
  -> f a
  -> g b
underF2 _ = coerce

-- | Similar to the function from the `Traversable` class, but operating within
-- | a newtype instead.
traverse
  :: forall f t a
   . Coercible (f a) (f t)
  => Newtype t a
  => (a -> t)
  -> (a -> f a)
  -> t
  -> f t
traverse _ = coerce

-- | Similar to the function from the `Distributive` class, but operating within
-- | a newtype instead.
collect
  :: forall f t a
   . Coercible (f a) (f t)
  => Newtype t a
  => (a -> t)
  -> (f a -> a)
  -> f t
  -> t
collect _ = coerce
