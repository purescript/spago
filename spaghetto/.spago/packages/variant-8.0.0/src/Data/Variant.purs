module Data.Variant
  ( Variant
  , inj
  , prj
  , on
  , onMatch
  , over
  , overOne
  , overSome
  , case_
  , match
  , default
  , traverse
  , traverseOne
  , traverseSome
  , expand
  , contract
  , Unvariant(..)
  , Unvariant'
  , unvariant
  , revariant
  , class VariantEqs, variantEqs
  , class VariantOrds, variantOrds
  , class VariantShows, variantShows
  , class VariantBounded, variantBounded
  , class VariantBoundedEnums, variantBoundedEnums
  , module Exports
  ) where

import Prelude

import Control.Alternative (empty, class Alternative)
import Data.Enum (class Enum, pred, succ, class BoundedEnum, Cardinality(..), fromEnum, toEnum, cardinality)
import Data.List as L
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant.Internal (class Contractable, class VariantMapCases, class VariantMatchCases, class VariantTraverseCases) as Exports
import Data.Variant.Internal (class Contractable, class VariantMapCases, class VariantMatchCases, class VariantTags, class VariantTraverseCases, BoundedDict, BoundedEnumDict, VariantCase, VariantRep(..), contractWith, lookup, lookupCardinality, lookupEq, lookupFirst, lookupFromEnum, lookupLast, lookupOrd, lookupPred, lookupSucc, lookupToEnum, unsafeGet, unsafeHas, variantTags)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as R
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Variant ∷ Row Type → Type

-- | Inject into the variant at a given label.
-- | ```purescript
-- | intAtFoo :: forall r. Variant (foo :: Int | r)
-- | intAtFoo = inj (Proxy :: Proxy "foo") 42
-- | ```
inj
  ∷ ∀ sym a r1 r2
  . R.Cons sym a r1 r2
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → a
  → Variant r2
inj p value = coerceV $ VariantRep { type: reflectSymbol p, value }
  where
  coerceV ∷ VariantRep a → Variant r2
  coerceV = unsafeCoerce

-- | Attempt to read a variant at a given label.
-- | ```purescript
-- | case prj (Proxy :: Proxy "foo") intAtFoo of
-- |   Just i  -> i + 1
-- |   Nothing -> 0
-- | ```
prj
  ∷ ∀ sym a r1 r2 f
  . R.Cons sym a r1 r2
  ⇒ IsSymbol sym
  ⇒ Alternative f
  ⇒ Proxy sym
  → Variant r2
  → f a
prj p = on p pure (const empty)

-- | Attempt to read a variant at a given label by providing branches.
-- | The failure branch receives the provided variant, but with the label
-- | removed.
on
  ∷ ∀ sym a b r1 r2
  . R.Cons sym a r1 r2
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → (a → b)
  → (Variant r1 → b)
  → Variant r2
  → b
on p f g r =
  case coerceV r of
    VariantRep v | v.type == reflectSymbol p → f v.value
    _ → g (coerceR r)
  where
  coerceV ∷ Variant r2 → VariantRep a
  coerceV = unsafeCoerce

  coerceR ∷ Variant r2 → Variant r1
  coerceR = unsafeCoerce

-- | Match a `Variant` with a `Record` containing functions for handling cases.
-- | This is similar to `on`, except instead of providing a single label and
-- | handler, you can provide a record where each field maps to a particular
-- | `Variant` case.
-- |
-- | ```purescript
-- | onMatch
-- |   { foo: \foo -> "Foo: " <> foo
-- |   , bar: \bar -> "Bar: " <> bar
-- |   }
-- | ```
-- |
-- | Polymorphic functions in records (such as `show` or `id`) can lead
-- | to inference issues if not all polymorphic variables are specified
-- | in usage. When in doubt, label methods with specific types, such as
-- | `show :: Int -> String`, or give the whole record an appropriate type.
onMatch
  ∷ ∀ rl r r1 r2 r3 b
  . RL.RowToList r rl
  ⇒ VariantMatchCases rl r1 b
  ⇒ R.Union r1 r2 r3
  ⇒ Record r
  → (Variant r2 → b)
  → Variant r3
  → b
onMatch r k v =
  case coerceV v of
    VariantRep v' | unsafeHas v'.type r → unsafeGet v'.type r v'.value
    _ → k (coerceR v)

  where
  coerceV ∷ ∀ a. Variant r3 → VariantRep a
  coerceV = unsafeCoerce

  coerceR ∷ Variant r3 → Variant r2
  coerceR = unsafeCoerce

-- | Map over one case of a variant, putting the result back at the same label,
-- | with a fallback function to handle the remaining cases.
overOne
  ∷ ∀ sym a b r1 r2 r3 r4
  . IsSymbol sym
  ⇒ R.Cons sym a r1 r2
  ⇒ R.Cons sym b r4 r3
  ⇒ Proxy sym
  → (a → b)
  → (Variant r1 → Variant r3)
  → Variant r2
  → Variant r3
overOne p f = on p (inj p <<< f)

-- | Map over several cases of a variant using a `Record` containing functions
-- | for each case. Each case gets put back at the same label it was matched
-- | at, i.e. its label in the record. Labels not found in the record are
-- | handled using the fallback function.
overSome
  ∷ ∀ r rl ri ro r1 r2 r3 r4
  . RL.RowToList r rl
  ⇒ VariantMapCases rl ri ro
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r4 r3
  ⇒ Record r
  → (Variant r2 → Variant r3)
  → Variant r1
  → Variant r3
overSome r k v =
  case coerceV v of
    VariantRep v' | unsafeHas v'.type r →
      coerceV' (VariantRep { type: v'.type, value: unsafeGet v'.type r v'.value })
    _ → k (coerceR v)

  where
  coerceV ∷ ∀ a. Variant r1 → VariantRep a
  coerceV = unsafeCoerce

  coerceV' ∷ ∀ a. VariantRep a → Variant r3
  coerceV' = unsafeCoerce

  coerceR ∷ Variant r1 → Variant r2
  coerceR = unsafeCoerce

-- | Map over some labels and leave the rest unchanged. For example:
-- |
-- | ```purescript
-- | over { label: show :: Int -> String }
-- |   :: forall r. Variant ( label :: Int | r ) -> Variant ( label :: String | r )
-- | ```
-- |
-- | `over r` is like `expand # overSome r` but with a more easily
-- | solved constraint (i.e. it can be solved once the type of `r` is known).
over
  ∷ ∀ r rl ri ro r1 r2 r3
  . RL.RowToList r rl
  ⇒ VariantMapCases rl ri ro
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r2 r3 -- this is "backwards" for `expand`, but still safe
  ⇒ Record r
  → Variant r1
  → Variant r3
over r = overSome r unsafeExpand where
  unsafeExpand = unsafeCoerce ∷ Variant r2 → Variant r3

-- | Traverse over one case of a variant (in a functorial/monadic context `m`),
-- | putting the result back at the same label, with a fallback function.
traverseOne
  ∷ ∀ sym a b r1 r2 r3 r4 m
  . IsSymbol sym
  ⇒ R.Cons sym a r1 r2
  ⇒ R.Cons sym b r4 r3
  ⇒ Functor m
  ⇒ Proxy sym
  → (a → m b)
  → (Variant r1 → m (Variant r3))
  → Variant r2
  → m (Variant r3)
traverseOne p f = on p (map (inj p) <<< f)

-- | Traverse over several cases of a variant using a `Record` containing
-- | traversals. Each case gets put back at the same label it was matched
-- | at, i.e. its label in the record. Labels not found in the record are
-- | handled using the fallback function.
traverseSome
  ∷ ∀ r rl ri ro r1 r2 r3 r4 m
  . RL.RowToList r rl
  ⇒ VariantTraverseCases m rl ri ro
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r4 r3
  ⇒ Functor m
  ⇒ Record r
  → (Variant r2 → m (Variant r3))
  → Variant r1
  → m (Variant r3)
traverseSome r k v =
  case coerceV v of
    VariantRep v' | unsafeHas v'.type r →
      unsafeGet v'.type r v'.value <#> \value ->
        coerceV' (VariantRep { type: v'.type, value })
    _ → k (coerceR v)

  where
  coerceV ∷ ∀ a. Variant r1 → VariantRep a
  coerceV = unsafeCoerce

  coerceV' ∷ ∀ a. VariantRep a → Variant r3
  coerceV' = unsafeCoerce

  coerceR ∷ Variant r1 → Variant r2
  coerceR = unsafeCoerce

-- | Traverse over some labels and leave the rest unchanged.
-- | (Implemented by expanding after `traverseSome`.)
traverse
  ∷ ∀ r rl ri ro r1 r2 r3 m
  . RL.RowToList r rl
  ⇒ VariantTraverseCases m rl ri ro
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r2 r3 -- this is "backwards" for `expand`, but still safe
  ⇒ Applicative m
  ⇒ Record r
  → Variant r1
  → m (Variant r3)
traverse r = traverseSome r (pure <<< unsafeExpand) where
  unsafeExpand = unsafeCoerce ∷ Variant r2 → Variant r3

-- | Combinator for exhaustive pattern matching.
-- | ```purescript
-- | caseFn :: Variant (foo :: Int, bar :: String, baz :: Boolean) -> String
-- | caseFn = case_
-- |  # on (Proxy :: Proxy "foo") (\foo -> "Foo: " <> show foo)
-- |  # on (Proxy :: Proxy "bar") (\bar -> "Bar: " <> bar)
-- |  # on (Proxy :: Proxy "baz") (\baz -> "Baz: " <> show baz)
-- | ```
case_ ∷ ∀ a. Variant () → a
case_ r = unsafeCrashWith case unsafeCoerce r of
  VariantRep v → "Data.Variant: pattern match failure [" <> v.type <> "]"

-- | Combinator for exhaustive pattern matching using an `onMatch` case record.
-- | ```purescript
-- | matchFn :: Variant (foo :: Int, bar :: String, baz :: Boolean) -> String
-- | matchFn = match
-- |   { foo: \foo -> "Foo: " <> show foo
-- |   , bar: \bar -> "Bar: " <> bar
-- |   , baz: \baz -> "Baz: " <> show baz
-- |   }
-- | ```
match
  ∷ ∀ rl r r1 r2 b
  . RL.RowToList r rl
  ⇒ VariantMatchCases rl r1 b
  ⇒ R.Union r1 () r2
  ⇒ Record r
  → Variant r2
  → b
match r = case_ # onMatch r

-- | Combinator for partial matching with a default value in case of failure.
-- | ```purescript
-- | caseFn :: forall r. Variant (foo :: Int, bar :: String | r) -> String
-- | caseFn = default "No match"
-- |  # on (Proxy :: Proxy "foo") (\foo -> "Foo: " <> show foo)
-- |  # on (Proxy :: Proxy "bar") (\bar -> "Bar: " <> bar)
-- | ```
default ∷ ∀ a r. a → Variant r → a
default a _ = a

-- | Every `Variant lt` can be cast to some `Variant gt` as long as `lt` is a
-- | subset of `gt`.
expand
  ∷ ∀ lt a gt
  . R.Union lt a gt
  ⇒ Variant lt
  → Variant gt
expand = unsafeCoerce

-- | A `Variant gt` can be cast to some `Variant lt`, where `lt` is is a subset
-- | of `gt`, as long as there is proof that the `Variant`'s runtime tag is
-- | within the subset of `lt`.
contract
  ∷ ∀ lt gt f
  . Alternative f
  ⇒ Contractable gt lt
  ⇒ Variant gt
  → f (Variant lt)
contract v =
  contractWith
    (Proxy ∷ Proxy gt)
    (Proxy ∷ Proxy lt)
    (case coerceV v of VariantRep v' → v'.type)
    (coerceR v)
  where
  coerceV ∷ ∀ a. Variant gt → VariantRep a
  coerceV = unsafeCoerce

  coerceR ∷ Variant gt → Variant lt
  coerceR = unsafeCoerce

type Unvariant' r x =
  ∀ s t o
  . IsSymbol s
  ⇒ R.Cons s t o r
  ⇒ Proxy s
  → t
  → x

newtype Unvariant r = Unvariant
  (∀ x. Unvariant' r x → x)

-- | A low-level eliminator which reifies the `IsSymbol` and `Cons`
-- | constraints required to reconstruct the Variant. This lets you
-- | work generically with some Variant at runtime.
unvariant
  ∷ ∀ r
  . Variant r
  → Unvariant r
unvariant v = case (unsafeCoerce v ∷ VariantRep Unit) of
  VariantRep o →
    Unvariant \f →
      coerce f { reflectSymbol: const o.type } {} Proxy o.value
  where
  coerce
    ∷ ∀ x
    . Unvariant' r x
    → { reflectSymbol ∷ Proxy "" → String }
    → {}
    → Proxy ""
    → Unit
    → x
  coerce = unsafeCoerce

-- | Reconstructs a Variant given an Unvariant eliminator.
revariant ∷ ∀ r. Unvariant r -> Variant r
revariant (Unvariant f) = f inj

class VariantEqs :: RL.RowList Type -> Constraint
class VariantEqs rl where
  variantEqs ∷ Proxy rl → L.List (VariantCase → VariantCase → Boolean)

instance eqVariantNil ∷ VariantEqs RL.Nil where
  variantEqs _ = L.Nil

instance eqVariantCons ∷ (VariantEqs rs, Eq a) ⇒ VariantEqs (RL.Cons sym a rs) where
  variantEqs _ =
    L.Cons (coerceEq eq) (variantEqs (Proxy ∷ Proxy rs))
    where
    coerceEq ∷ (a → a → Boolean) → VariantCase → VariantCase → Boolean
    coerceEq = unsafeCoerce

instance eqVariant ∷ (RL.RowToList r rl, VariantTags rl, VariantEqs rl) ⇒ Eq (Variant r) where
  eq v1 v2 =
    let
      c1 = unsafeCoerce v1 ∷ VariantRep VariantCase
      c2 = unsafeCoerce v2 ∷ VariantRep VariantCase
      tags = variantTags (Proxy ∷ Proxy rl)
      eqs = variantEqs (Proxy ∷ Proxy rl)
    in
      lookupEq tags eqs c1 c2

class VariantBounded :: RL.RowList Type -> Constraint
class VariantBounded rl where
  variantBounded ∷ Proxy rl → L.List (BoundedDict VariantCase)

instance boundedVariantNil ∷ VariantBounded RL.Nil where
  variantBounded _ = L.Nil

instance boundedVariantCons ∷ (VariantBounded rs, Bounded a) ⇒ VariantBounded (RL.Cons sym a rs) where
  variantBounded _ = L.Cons dict (variantBounded (Proxy ∷ Proxy rs))
    where
    dict ∷ BoundedDict VariantCase
    dict =
      { top: coerce top
      , bottom: coerce bottom
      }

    coerce ∷ a → VariantCase
    coerce = unsafeCoerce

instance boundedVariant ∷ (RL.RowToList r rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBounded rl) ⇒ Bounded (Variant r) where
  top =
    let
      tags = variantTags (Proxy ∷ Proxy rl)
      dicts = variantBounded (Proxy ∷ Proxy rl)
      coerce = unsafeCoerce ∷ VariantRep VariantCase → Variant r
    in
      coerce $ VariantRep $ lookupLast "top" _.top tags dicts

  bottom =
    let
      tags = variantTags (Proxy ∷ Proxy rl)
      dicts = variantBounded (Proxy ∷ Proxy rl)
      coerce = unsafeCoerce ∷ VariantRep VariantCase → Variant r
    in
      coerce $ VariantRep $ lookupFirst "bottom" _.bottom tags dicts

class VariantBoundedEnums :: RL.RowList Type -> Constraint
class VariantBounded rl ⇐ VariantBoundedEnums rl where
  variantBoundedEnums ∷ Proxy rl → L.List (BoundedEnumDict VariantCase)

instance enumVariantNil ∷ VariantBoundedEnums RL.Nil where
  variantBoundedEnums _ = L.Nil

instance enumVariantCons ∷ (VariantBoundedEnums rs, BoundedEnum a) ⇒ VariantBoundedEnums (RL.Cons sym a rs) where
  variantBoundedEnums _ = L.Cons dict (variantBoundedEnums (Proxy ∷ Proxy rs))
    where
    dict ∷ BoundedEnumDict VariantCase
    dict =
      { pred: coerceAToMbA pred
      , succ: coerceAToMbA succ
      , fromEnum: coerceFromEnum fromEnum
      , toEnum: coerceToEnum toEnum
      , cardinality: coerceCardinality cardinality
      }

    coerceAToMbA ∷ (a → Maybe a) → VariantCase → Maybe VariantCase
    coerceAToMbA = unsafeCoerce

    coerceFromEnum ∷ (a → Int) → VariantCase → Int
    coerceFromEnum = unsafeCoerce

    coerceToEnum ∷ (Int → Maybe a) → Int → Maybe VariantCase
    coerceToEnum = unsafeCoerce

    coerceCardinality ∷ Cardinality a → Int
    coerceCardinality = unsafeCoerce

instance enumVariant ∷ (RL.RowToList r rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBoundedEnums rl) ⇒ Enum (Variant r) where
  pred a =
    let
      rep = unsafeCoerce a ∷ VariantRep VariantCase
      tags = variantTags (Proxy ∷ Proxy rl)
      bounds = variantBounded (Proxy ∷ Proxy rl)
      dicts = variantBoundedEnums (Proxy ∷ Proxy rl)
      coerce = unsafeCoerce ∷ Maybe (VariantRep VariantCase) → Maybe (Variant r)
    in
      coerce $ lookupPred rep tags bounds dicts

  succ a =
    let
      rep = unsafeCoerce a ∷ VariantRep VariantCase
      tags = variantTags (Proxy ∷ Proxy rl)
      bounds = variantBounded (Proxy ∷ Proxy rl)
      dicts = variantBoundedEnums (Proxy ∷ Proxy rl)
      coerce = unsafeCoerce ∷ Maybe (VariantRep VariantCase) → Maybe (Variant r)
    in
      coerce $ lookupSucc rep tags bounds dicts

instance boundedEnumVariant ∷ (RL.RowToList r rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBoundedEnums rl) ⇒ BoundedEnum (Variant r) where
  cardinality =
    Cardinality $ lookupCardinality $ variantBoundedEnums (Proxy ∷ Proxy rl)

  fromEnum a =
    let
      rep = unsafeCoerce a ∷ VariantRep VariantCase
      tags = variantTags (Proxy ∷ Proxy rl)
      dicts = variantBoundedEnums (Proxy ∷ Proxy rl)
    in
      lookupFromEnum rep tags dicts

  toEnum n =
    let
      tags = variantTags (Proxy ∷ Proxy rl)
      dicts = variantBoundedEnums (Proxy ∷ Proxy rl)
      coerceV = unsafeCoerce ∷ Maybe (VariantRep VariantCase) → Maybe (Variant r)
    in
      coerceV $ lookupToEnum n tags dicts

class VariantOrds :: RL.RowList Type -> Constraint
class VariantOrds rl where
  variantOrds ∷ Proxy rl → L.List (VariantCase → VariantCase → Ordering)

instance ordVariantNil ∷ VariantOrds RL.Nil where
  variantOrds _ = L.Nil

instance ordVariantCons ∷ (VariantOrds rs, Ord a) ⇒ VariantOrds (RL.Cons sym a rs) where
  variantOrds _ =
    L.Cons (coerceOrd compare) (variantOrds (Proxy ∷ Proxy rs))
    where
    coerceOrd ∷ (a → a → Ordering) → VariantCase → VariantCase → Ordering
    coerceOrd = unsafeCoerce

instance ordVariant ∷ (RL.RowToList r rl, VariantTags rl, VariantEqs rl, VariantOrds rl) ⇒ Ord (Variant r) where
  compare v1 v2 =
    let
      c1 = unsafeCoerce v1 ∷ VariantRep VariantCase
      c2 = unsafeCoerce v2 ∷ VariantRep VariantCase
      tags = variantTags (Proxy ∷ Proxy rl)
      ords = variantOrds (Proxy ∷ Proxy rl)
    in
      lookupOrd tags ords c1 c2

class VariantShows :: RL.RowList Type -> Constraint
class VariantShows rl where
  variantShows ∷ Proxy rl → L.List (VariantCase → String)

instance showVariantNil ∷ VariantShows RL.Nil where
  variantShows _ = L.Nil

instance showVariantCons ∷ (VariantShows rs, Show a) ⇒ VariantShows (RL.Cons sym a rs) where
  variantShows _ =
    L.Cons (coerceShow show) (variantShows (Proxy ∷ Proxy rs))
    where
    coerceShow ∷ (a → String) → VariantCase → String
    coerceShow = unsafeCoerce

instance showVariant ∷ (RL.RowToList r rl, VariantTags rl, VariantShows rl) ⇒ Show (Variant r) where
  show v1 =
    let
      VariantRep v = unsafeCoerce v1 ∷ VariantRep VariantCase
      tags = variantTags (Proxy ∷ Proxy rl)
      shows = variantShows (Proxy ∷ Proxy rl)
      body = lookup "show" v.type tags shows v.value
    in
      "(inj @" <> show v.type <> " " <> body <> ")"
