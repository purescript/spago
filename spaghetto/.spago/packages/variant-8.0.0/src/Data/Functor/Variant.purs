module Data.Functor.Variant
  ( VariantF
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
  , UnvariantF(..)
  , UnvariantF'
  , unvariantF
  , revariantF
  , class VariantFShows, variantFShows
  , class VariantFMaps, variantFMaps, Mapper
  , class TraversableVFRL
  , class FoldableVFRL
  , traverseVFRL
  , foldrVFRL
  , foldlVFRL
  , foldMapVFRL
  , module Exports
  ) where

import Prelude

import Control.Alternative (class Alternative, empty)
import Data.List as L
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable as TF
import Data.Variant.Internal (class Contractable, class VariantFMatchCases, class VariantFMapCases) as Exports
import Data.Variant.Internal (class Contractable, class VariantFMapCases, class VariantFMatchCases, class VariantFTraverseCases, class VariantTags, VariantFCase, VariantCase, contractWith, lookup, unsafeGet, unsafeHas, variantTags)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as R
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Mapper f = Mapper (forall a b. (a → b) → f a → f b)

newtype VariantFRep f a = VariantFRep
  { type ∷ String
  , value ∷ f a
  , map ∷ Mapper f
  }

data UnknownF :: Type -> Type
data UnknownF a

data VariantF :: Row (Type -> Type) -> Type -> Type
data VariantF f a

instance functorVariantF ∷ Functor (VariantF r) where
  map f a =
    case coerceY a of
      VariantFRep v → coerceV $ VariantFRep
        { type: v.type
        , value: case v.map of Mapper m → m f v.value
        , map: v.map
        }
    where
    coerceY ∷ ∀ f a. VariantF r a → VariantFRep f a
    coerceY = unsafeCoerce

    coerceV ∷ ∀ f a. VariantFRep f a → VariantF r a
    coerceV = unsafeCoerce

class FoldableVFRL :: RL.RowList (Type -> Type) -> Row (Type -> Type) -> Constraint
class FoldableVFRL rl row | rl -> row where
  foldrVFRL :: forall a b. Proxy rl -> (a -> b -> b) -> b -> VariantF row a -> b
  foldlVFRL :: forall a b. Proxy rl -> (b -> a -> b) -> b -> VariantF row a -> b
  foldMapVFRL :: forall a m. Monoid m => Proxy rl -> (a -> m) -> VariantF row a -> m

instance foldableNil :: FoldableVFRL RL.Nil () where
  foldrVFRL _ _ _ = case_
  foldlVFRL _ _ _ = case_
  foldMapVFRL _ _ = case_

instance foldableCons ::
  ( IsSymbol k
  , TF.Foldable f
  , FoldableVFRL rl r
  , R.Cons k f r r'
  ) => FoldableVFRL (RL.Cons k f rl) r' where
  foldrVFRL _ f b = on k (TF.foldr f b) (foldrVFRL (Proxy :: Proxy rl) f b)
    where k = Proxy :: Proxy k
  foldlVFRL _ f b = on k (TF.foldl f b) (foldlVFRL (Proxy :: Proxy rl) f b)
    where k = Proxy :: Proxy k
  foldMapVFRL _ f = on k (TF.foldMap f) (foldMapVFRL (Proxy :: Proxy rl) f)
    where k = Proxy :: Proxy k

class TraversableVFRL :: RL.RowList (Type -> Type) -> Row (Type -> Type) -> Constraint
class FoldableVFRL rl row <= TraversableVFRL rl row | rl -> row where
  traverseVFRL :: forall f a b. Applicative f => Proxy rl -> (a -> f b) -> VariantF row a -> f (VariantF row b)

instance traversableNil :: TraversableVFRL RL.Nil () where
  traverseVFRL _ _ = case_

instance traversableCons ::
  ( IsSymbol k
  , TF.Traversable f
  , TraversableVFRL rl r
  , R.Cons k f r r'
  , R.Union r rx r'
  ) => TraversableVFRL (RL.Cons k f rl) r' where
  traverseVFRL _ f = on k (TF.traverse f >>> map (inj k))
    (traverseVFRL (Proxy :: Proxy rl) f >>> map expand)
    where k = Proxy :: Proxy k

instance foldableVariantF ::
  (RL.RowToList row rl, FoldableVFRL rl row) =>
  TF.Foldable (VariantF row) where
    foldr = foldrVFRL (Proxy :: Proxy rl)
    foldl = foldlVFRL (Proxy :: Proxy rl)
    foldMap = foldMapVFRL (Proxy :: Proxy rl)

instance traversableVariantF ::
  (RL.RowToList row rl, TraversableVFRL rl row) =>
  TF.Traversable (VariantF row) where
    traverse = traverseVFRL (Proxy :: Proxy rl)
    sequence = TF.sequenceDefault

-- | Inject into the variant at a given label.
-- | ```purescript
-- | maybeAtFoo :: forall r. VariantF (foo :: Maybe | r) Int
-- | maybeAtFoo = inj (Proxy :: Proxy "foo") (Just 42)
-- | ```
inj
  ∷ ∀ sym f a r1 r2
  . R.Cons sym f r1 r2
  ⇒ IsSymbol sym
  ⇒ Functor f
  ⇒ Proxy sym
  → f a
  → VariantF r2 a
inj p value = coerceV $ VariantFRep { type: reflectSymbol p, value, map: Mapper map }
  where
  coerceV ∷ VariantFRep f a → VariantF r2 a
  coerceV = unsafeCoerce

-- | Attempt to read a variant at a given label.
-- | ```purescript
-- | case prj (Proxy :: Proxy "foo") maybeAtFoo of
-- |   Just (Just i) -> i + 1
-- |   _ -> 0
-- | ```
prj
  ∷ ∀ sym f a r1 r2 g
  . R.Cons sym f r1 r2
  ⇒ Alternative g
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → VariantF r2 a
  → g (f a)
prj p = on p pure (const empty)

-- | Attempt to read a variant at a given label by providing branches.
-- | The failure branch receives the provided variant, but with the label
-- | removed.
on
  ∷ ∀ sym f a b r1 r2
  . R.Cons sym f r1 r2
  ⇒ IsSymbol sym
  ⇒ Proxy sym
  → (f a → b)
  → (VariantF r1 a → b)
  → VariantF r2 a
  → b
on p f g r =
  case coerceY r of
    VariantFRep v | v.type == reflectSymbol p → f v.value
    _ → g (coerceR r)
  where
  coerceY ∷ VariantF r2 a → VariantFRep f a
  coerceY = unsafeCoerce

  coerceR ∷ VariantF r2 a → VariantF r1 a
  coerceR = unsafeCoerce

-- | Match a `VariantF` with a `Record` containing functions for handling cases.
-- | This is similar to `on`, except instead of providing a single label and
-- | handler, you can provide a record where each field maps to a particular
-- | `VariantF` case.
-- |
-- | ```purescript
-- | onMatch
-- |  { foo: \foo -> "Foo: " <> maybe "nothing" id foo
-- |  , bar: \bar -> "Bar: " <> snd bar
-- |  }
-- | ```
-- |
-- | Polymorphic functions in records (such as `show` or `id`) can lead
-- | to inference issues if not all polymorphic variables are specified
-- | in usage. When in doubt, label methods with specific types, such as
-- | `show :: Int -> String`, or give the whole record an appropriate type.
onMatch
  ∷ ∀ rl r r1 r2 r3 a b
  . RL.RowToList r rl
  ⇒ VariantFMatchCases rl r1 a b
  ⇒ R.Union r1 r2 r3
  ⇒ Record r
  → (VariantF r2 a → b)
  → VariantF r3 a
  → b
onMatch r k v =
  case coerceV v of
    VariantFRep v' | unsafeHas v'.type r → unsafeGet v'.type r v'.value
    _ → k (coerceR v)

  where
  coerceV ∷ ∀ f. VariantF r3 a → VariantFRep f a
  coerceV = unsafeCoerce

  coerceR ∷ VariantF r3 a → VariantF r2 a
  coerceR = unsafeCoerce

-- | Map over one case of a variant, putting the result back at the same label,
-- | with a fallback function to handle the remaining cases.
overOne
  ∷ ∀ sym f g a b r1 r2 r3 r4
  . R.Cons sym f r1 r2
  ⇒ R.Cons sym g r4 r3
  ⇒ IsSymbol sym
  ⇒ Functor g
  ⇒ Proxy sym
  → (f a → g b)
  → (VariantF r1 a → VariantF r3 b)
  → VariantF r2 a
  → VariantF r3 b
overOne p f = on p (inj p <<< f)

-- | Map over several cases of a variant using a `Record` containing functions
-- | for each case. Each case gets put back at the same label it was matched
-- | at, i.e. its label in the record. Labels not found in the record are
-- | handled using the fallback function.
overSome
  ∷ ∀ r rl rlo ri ro r1 r2 r3 r4 a b
  . RL.RowToList r rl
  ⇒ VariantFMapCases rl ri ro a b
  ⇒ RL.RowToList ro rlo
  ⇒ VariantTags rlo
  ⇒ VariantFMaps rlo
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r4 r3
  ⇒ Record r
  → (VariantF r2 a → VariantF r3 b)
  → VariantF r1 a
  → VariantF r3 b
overSome r k v =
  case coerceV v of
    VariantFRep v' | unsafeHas v'.type r →
      let
        tags = variantTags (Proxy ∷ Proxy rlo)
        maps = variantFMaps (Proxy ∷ Proxy rlo)
        map = lookup "map" v'.type tags maps
      in coerceV' (VariantFRep { type: v'.type, map, value: unsafeGet v'.type r v'.value })
    _ → k (coerceR v)

  where
  coerceV ∷ ∀ f. VariantF r1 a → VariantFRep f a
  coerceV = unsafeCoerce

  coerceV' ∷ ∀ g. VariantFRep g b → VariantF r3 b
  coerceV' = unsafeCoerce

  coerceR ∷ VariantF r1 a → VariantF r2 a
  coerceR = unsafeCoerce

-- | Map over some labels (with access to the containers) and use `map f` for
-- | the rest (just changing the index type). For example:
-- |
-- | ```purescript
-- | over { label: \(Identity a) -> Just (show (a - 5)) } show
-- |   :: forall r.
-- |     VariantF ( label :: Identity | r ) Int ->
-- |     VariantF ( label :: Maybe | r ) String
-- | ```
-- |
-- | `over r f` is like `(map f >>> expand) # overSome r` but with
-- | a more easily solved constraint (i.e. it can be solved once the type of
-- | `r` is known).
over
  ∷ ∀ r rl rlo ri ro r1 r2 r3 a b
  . RL.RowToList r rl
  ⇒ VariantFMapCases rl ri ro a b
  ⇒ RL.RowToList ro rlo
  ⇒ VariantTags rlo
  ⇒ VariantFMaps rlo
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r2 r3 -- this is "backwards" for `expand`, but still safe
  ⇒ Record r
  → (a → b)
  → VariantF r1 a
  → VariantF r3 b
over r f = overSome r (map f >>> unsafeExpand) where
  unsafeExpand = unsafeCoerce ∷ VariantF r2 b → VariantF r3 b

-- | Traverse over one case of a variant (in a functorial/monadic context `m`),
-- | putting the result back at the same label, with a fallback function.
traverseOne
  ∷ ∀ sym f g a b r1 r2 r3 r4 m
  . R.Cons sym f r1 r2
  ⇒ R.Cons sym g r4 r3
  ⇒ IsSymbol sym
  ⇒ Functor g
  ⇒ Functor m
  ⇒ Proxy sym
  → (f a → m (g b))
  → (VariantF r1 a → m (VariantF r3 b))
  → VariantF r2 a
  → m (VariantF r3 b)
traverseOne p f = on p (map (inj p) <<< f)

-- | Traverse over several cases of a variant using a `Record` containing
-- | traversals. Each case gets put back at the same label it was matched
-- | at, i.e. its label in the record. Labels not found in the record are
-- | handled using the fallback function.
traverseSome
  ∷ ∀ r rl rlo ri ro r1 r2 r3 r4 a b m
  . RL.RowToList r rl
  ⇒ VariantFTraverseCases m rl ri ro a b
  ⇒ RL.RowToList ro rlo
  ⇒ VariantTags rlo
  ⇒ VariantFMaps rlo
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r4 r3
  ⇒ Functor m
  ⇒ Record r
  → (VariantF r2 a → m (VariantF r3 b))
  → VariantF r1 a
  → m (VariantF r3 b)
traverseSome r k v =
  case coerceV v of
    VariantFRep v' | unsafeHas v'.type r →
      let
        tags = variantTags (Proxy ∷ Proxy rlo)
        maps = variantFMaps (Proxy ∷ Proxy rlo)
        map = lookup "map" v'.type tags maps
      in unsafeGet v'.type r v'.value <#> \value ->
          coerceV' (VariantFRep { type: v'.type, map, value })
    _ → k (coerceR v)

  where
  coerceV ∷ ∀ f. VariantF r1 a → VariantFRep f a
  coerceV = unsafeCoerce

  coerceV' ∷ ∀ g. VariantFRep g b → VariantF r3 b
  coerceV' = unsafeCoerce

  coerceR ∷ VariantF r1 a → VariantF r2 a
  coerceR = unsafeCoerce

-- | Traverse over some labels (with access to the containers) and use
-- | `traverse f` for the rest (just changing the index type).
-- |
-- | `traverse r f` is like `(traverse f >>> expand) # traverseSome r` but with
-- | a more easily solved constraint (i.e. it can be solved once the type of
-- | `r` is known).
traverse
  ∷ ∀ r rl rlo ri ro r1 r2 r3 a b m
  . RL.RowToList r rl
  ⇒ VariantFTraverseCases m rl ri ro a b
  ⇒ RL.RowToList ro rlo
  ⇒ VariantTags rlo
  ⇒ VariantFMaps rlo
  ⇒ R.Union ri r2 r1
  ⇒ R.Union ro r2 r3 -- this is "backwards" for `expand`, but still safe
  ⇒ Applicative m
  ⇒ TF.Traversable (VariantF r2)
  ⇒ Record r
  → (a → m b)
  → VariantF r1 a
  → m (VariantF r3 b)
traverse r f = traverseSome r (TF.traverse f >>> map unsafeExpand) where
  unsafeExpand = unsafeCoerce ∷ VariantF r2 b → VariantF r3 b

-- | Combinator for exhaustive pattern matching.
-- | ```purescript
-- | caseFn :: VariantF (foo :: Maybe, bar :: Tuple String, baz :: Either String) Int -> String
-- | caseFn = case_
-- |  # on (Proxy :: Proxy "foo") (\foo -> "Foo: " <> maybe "nothing" show foo)
-- |  # on (Proxy :: Proxy "bar") (\bar -> "Bar: " <> show (snd bar))
-- |  # on (Proxy :: Proxy "baz") (\baz -> "Baz: " <> either id show baz)
-- | ```
case_ ∷ ∀ a b. VariantF () a → b
case_ r = unsafeCrashWith case unsafeCoerce r of
  VariantFRep v → "Data.Functor.Variant: pattern match failure [" <> v.type <> "]"

-- | Combinator for exhaustive pattern matching using an `onMatch` case record.
-- | ```purescript
-- | matchFn :: VariantF (foo :: Maybe, bar :: Tuple String, baz :: Either String) Int -> String
-- | matchFn = match
-- |  { foo: \foo -> "Foo: " <> maybe "nothing" show foo
-- |  , bar: \bar -> "Bar: " <> show (snd bar)
-- |  , baz: \baz -> "Baz: " <> either id show baz
-- |  }
-- | ```
match
  ∷ ∀ rl r r1 r2 a b
  . RL.RowToList r rl
  ⇒ VariantFMatchCases rl r1 a b
  ⇒ R.Union r1 () r2
  ⇒ Record r
  → VariantF r2 a
  → b
match r = case_ # onMatch r

-- | Combinator for partial matching with a default value in case of failure.
-- | ```purescript
-- | caseFn :: forall r. VariantF (foo :: Maybe, bar :: Tuple String | r) Int -> String
-- | caseFn = default "No match"
-- |  # on (Proxy :: Proxy "foo") (\foo -> "Foo: " <> maybe "nothing" show foo)
-- |  # on (Proxy :: Proxy "bar") (\bar -> "Bar: " <> show (snd bar))
-- | ```
default ∷ ∀ a b r. a → VariantF r b → a
default a _ = a

-- | Every `VariantF lt a` can be cast to some `VariantF gt a` as long as `lt` is a
-- | subset of `gt`.
expand
  ∷ ∀ lt mix gt a
  . R.Union lt mix gt
  ⇒ VariantF lt a
  → VariantF gt a
expand = unsafeCoerce

-- | A `VariantF gt a` can be cast to some `VariantF lt a`, where `lt` is is a subset
-- | of `gt`, as long as there is proof that the `VariantF`'s runtime tag is
-- | within the subset of `lt`.
contract
  ∷ ∀ lt gt f a
  . Alternative f
  ⇒ Contractable gt lt
  ⇒ VariantF gt a
  → f (VariantF lt a)
contract v =
  contractWith
    (Proxy ∷ Proxy gt)
    (Proxy ∷ Proxy lt)
    (case coerceV v of VariantFRep v' → v'.type)
    (coerceR v)
  where
  coerceV ∷ ∀ g. VariantF gt a → VariantFRep g a
  coerceV = unsafeCoerce

  coerceR ∷ VariantF gt a → VariantF lt a
  coerceR = unsafeCoerce

type UnvariantF' r a x =
  ∀ s f o
  . IsSymbol s
  ⇒ R.Cons s f o r
  ⇒ Functor f
  ⇒ Proxy s
  → f a
  → x

newtype UnvariantF r a = UnvariantF
  (∀ x. UnvariantF' r a x → x)

-- | A low-level eliminator which reifies the `IsSymbol`, `Cons` and
-- | `Functor` constraints require to reconstruct the Variant. This
-- | lets you work generically with some VariantF at runtime.
unvariantF
  ∷ ∀ r a
  . VariantF r a
  → UnvariantF r a
unvariantF v = case (unsafeCoerce v ∷ VariantFRep UnknownF Unit) of
  VariantFRep o →
    UnvariantF \f →
      coerce f
        { reflectSymbol: const o.type }
        {}
        { map: o.map }
        Proxy
        o.value
  where
  coerce
    ∷ ∀ x
    . UnvariantF' r a x
    → { reflectSymbol ∷ Proxy "" → String }
    → {}
    → { map ∷ Mapper UnknownF }
    → Proxy ""
    → UnknownF Unit
    → x
  coerce = unsafeCoerce

-- | Reconstructs a VariantF given an UnvariantF eliminator.
revariantF ∷ ∀ r a. UnvariantF r a -> VariantF r a
revariantF (UnvariantF f) = f inj

class VariantFShows :: RL.RowList (Type -> Type) -> Type -> Constraint
class VariantFShows rl x where
  variantFShows ∷ forall proxy1 proxy2. proxy1 rl → proxy2 x → L.List (VariantCase → String)

instance showVariantFNil ∷ VariantFShows RL.Nil x where
  variantFShows _ _ = L.Nil

instance showVariantFCons ∷ (VariantFShows rs x, Show (f x), Show x) ⇒ VariantFShows (RL.Cons sym f rs) x where
  variantFShows _ p =
    L.Cons (coerceShow show) (variantFShows (Proxy ∷ Proxy rs) p)
    where
    coerceShow ∷ (f x → String) → VariantCase → String
    coerceShow = unsafeCoerce

instance showVariantF ∷ (RL.RowToList r rl, VariantTags rl, VariantFShows rl a, Show a) ⇒ Show (VariantF r a) where
  show v1 =
    let
      VariantFRep v = unsafeCoerce v1 ∷ VariantFRep VariantFCase a
      tags = variantTags (Proxy ∷ Proxy rl)
      shows = variantFShows (Proxy ∷ Proxy rl) (Proxy ∷ Proxy a)
      body = lookup "show" v.type tags shows (unsafeCoerce v.value ∷ VariantCase)
    in
      "(inj @" <> show v.type <> " " <> body <> ")"

class VariantFMaps (rl ∷ RL.RowList (Type → Type)) where
  variantFMaps ∷ Proxy rl → L.List (Mapper VariantFCase)

instance mapVariantFNil ∷ VariantFMaps RL.Nil where
  variantFMaps _ = L.Nil

instance mapVariantFCons ∷ (VariantFMaps rs, Functor f) ⇒ VariantFMaps (RL.Cons sym f rs) where
  variantFMaps _ =
    L.Cons (coerceMap (Mapper map)) (variantFMaps (Proxy ∷ Proxy rs))
    where
    coerceMap ∷ Mapper f → Mapper VariantFCase
    coerceMap = unsafeCoerce
