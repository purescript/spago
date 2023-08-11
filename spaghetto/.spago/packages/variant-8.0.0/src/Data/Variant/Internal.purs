module Data.Variant.Internal
  ( VariantRep(..)
  , VariantCase
  , VariantFCase
  , class VariantTags, variantTags
  , class Contractable, contractWith
  , class VariantMatchCases
  , class VariantFMatchCases
  , class VariantMapCases
  , class VariantFMapCases
  , class VariantTraverseCases
  , class VariantFTraverseCases
  , lookup
  , lookupTag
  , lookupEq
  , lookupOrd
  , lookupLast
  , lookupFirst
  , lookupPred
  , lookupSucc
  , lookupCardinality
  , lookupFromEnum
  , lookupToEnum
  , BoundedDict
  , BoundedEnumDict
  , impossible
  , module Exports
  ) where

import Prelude

import Control.Alternative (class Alternative, empty)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as R
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeHas) as Exports
import Type.Proxy (Proxy(..))
import Type.Equality (class TypeEquals)

newtype VariantRep a = VariantRep
  { type ∷ String
  , value ∷ a
  }

class VariantMatchCases :: RL.RowList Type -> Row Type -> Type -> Constraint
class VariantMatchCases rl vo b | rl → vo b

instance variantMatchCons
  ∷ ( VariantMatchCases rl vo' b
    , R.Cons sym a vo' vo
    , TypeEquals k (a → b)
    )
  ⇒ VariantMatchCases (RL.Cons sym k rl) vo b

instance variantMatchNil
  ∷ VariantMatchCases RL.Nil () b

class VariantFMatchCases :: RL.RowList Type -> Row (Type -> Type) -> Type -> Type -> Constraint
class VariantFMatchCases rl vo a b | rl → vo a b

instance variantFMatchCons
  ∷ ( VariantFMatchCases rl vo' a b
    , R.Cons sym f vo' vo
    , TypeEquals k (f a → b)
    )
  ⇒ VariantFMatchCases (RL.Cons sym k rl) vo a b

instance variantFMatchNil
  ∷ VariantFMatchCases RL.Nil () a b

class VariantMapCases (rl ∷ RL.RowList Type)
  (ri ∷ Row Type) (ro ∷ Row Type)
  | rl → ri ro

instance variantMapCons
  ∷ ( R.Cons sym a ri' ri
    , R.Cons sym b ro' ro
    , VariantMapCases rl ri' ro'
    , TypeEquals k (a → b)
    )
  ⇒ VariantMapCases (RL.Cons sym k rl) ri ro

instance variantMapNil
  ∷ VariantMapCases RL.Nil () ()

class VariantFMapCases (rl ∷ RL.RowList Type)
  (ri ∷ Row (Type → Type)) (ro ∷ Row (Type → Type)) (a ∷ Type) (b ∷ Type)
  | rl → ri ro

instance variantFMapCons
  ∷ ( R.Cons sym f ri' ri
    , R.Cons sym g ro' ro
    , VariantFMapCases rl ri' ro' a b
    , TypeEquals k (f a → g b)
    )
  ⇒ VariantFMapCases (RL.Cons sym k rl) ri ro a b

instance variantFMapNil
  ∷ VariantFMapCases RL.Nil () () a b

class VariantTraverseCases (m ∷ Type → Type) (rl ∷ RL.RowList Type)
  (ri ∷ Row Type) (ro ∷ Row Type)
  | rl → ri ro

instance variantTravCons
  ∷ ( R.Cons sym a ri' ri
    , R.Cons sym b ro' ro
    , VariantTraverseCases m rl ri' ro'
    , TypeEquals k (a → m b)
    )
  ⇒ VariantTraverseCases m (RL.Cons sym k rl) ri ro

instance variantTravNil
  ∷ VariantTraverseCases m RL.Nil () ()

class VariantFTraverseCases (m ∷ Type → Type) (rl ∷ RL.RowList Type)
  (ri ∷ Row (Type → Type)) (ro ∷ Row (Type → Type)) (a ∷ Type) (b ∷ Type)
  | rl → ri ro

instance variantFTravCons
  ∷ ( R.Cons sym f ri' ri
    , R.Cons sym g ro' ro
    , VariantFTraverseCases m rl ri' ro' a b
    , TypeEquals k (f a → m (g b))
    )
  ⇒ VariantFTraverseCases m (RL.Cons sym k rl) ri ro a b

instance variantFTravNil
  ∷ VariantFTraverseCases m RL.Nil () () a b

foreign import data VariantCase ∷ Type

foreign import data VariantFCase ∷ Type → Type

class VariantTags :: forall k. RL.RowList k -> Constraint
class VariantTags rl where
  variantTags ∷ Proxy rl → L.List String

instance variantTagsNil ∷ VariantTags RL.Nil where
  variantTags _ = L.Nil

instance variantTagsCons ∷ (VariantTags rs, IsSymbol sym) ⇒ VariantTags (RL.Cons sym a rs) where
  variantTags _ = L.Cons (reflectSymbol (Proxy ∷ Proxy sym)) (variantTags (Proxy ∷ Proxy rs))

-- | A specialized lookup function which bails early. Foldable's `elem`
-- | is always worst-case.
lookupTag ∷ String → L.List String → Boolean
lookupTag tag = go
  where
  go = case _ of
    t L.: ts
      | t == tag → true
      | otherwise → go ts
    L.Nil → false

lookupEq
  ∷ L.List String
  → L.List (VariantCase → VariantCase → Boolean)
  → VariantRep VariantCase
  → VariantRep VariantCase
  → Boolean
lookupEq tags eqs (VariantRep v1) (VariantRep v2)
  | v1.type == v2.type = lookup "eq" v1.type tags eqs v1.value v2.value
  | otherwise = false

lookupOrd
  ∷ L.List String
  → L.List (VariantCase → VariantCase → Ordering)
  → VariantRep VariantCase
  → VariantRep VariantCase
  → Ordering
lookupOrd tags ords (VariantRep v1) (VariantRep v2) =
  case compare v1.type v2.type of
    EQ → lookup "compare" v1.type tags ords v1.value v2.value
    cp → cp

lookup
  ∷ ∀ a
  . String
  → String
  → L.List String
  → L.List a
  → a
lookup name tag = go
  where
  go = case _, _ of
    L.Cons t ts, L.Cons f fs
      | t == tag → f
      | otherwise → go ts fs
    _, _ → impossible name

lookupLast
  ∷ ∀ a b
  . String
  → (a → b)
  → L.List String
  → L.List a
  → { type ∷ String, value ∷ b }
lookupLast name f = go
  where
  go = case _, _ of
    L.Cons t L.Nil, L.Cons x L.Nil → { type: t, value: f x }
    L.Cons _ ts, L.Cons _ xs → go ts xs
    _, _ → impossible name

lookupFirst
  ∷ ∀ a b
  . String
  → (a → b)
  → L.List String
  → L.List a
  → { type ∷ String, value ∷ b }
lookupFirst name f = go
  where
  go = case _, _ of
    L.Cons t _, L.Cons x _ → { type: t, value: f x }
    _, _ → impossible name

lookupPred
  ∷ ∀ a
  . VariantRep a
  → L.List String
  → L.List (BoundedDict a)
  → L.List (BoundedEnumDict a)
  → Maybe (VariantRep a)
lookupPred (VariantRep rep) = go1
  where
  go1 = case _, _, _ of
    L.Cons t1 ts1, L.Cons b1 bs1, L.Cons d1 ds1
      | t1 == rep.type →
          case d1.pred rep.value of
            Nothing → Nothing
            Just z  → Just $ VariantRep { type: rep.type, value: z }
      | otherwise → go2 t1 b1 d1 ts1 bs1 ds1
    _, _, _ → impossible "pred"

  go2 t1 b1 _ = case _, _, _ of
    L.Cons t2 ts2, L.Cons b2 bs2, L.Cons d2 ds2
      | t2 == rep.type →
          case d2.pred rep.value of
            Nothing → Just $ VariantRep { type: t1, value: b1.top }
            Just z  → Just $ VariantRep { type: rep.type, value: z }
      | otherwise → go2 t2 b2 d2 ts2 bs2 ds2
    _, _, _ → impossible "pred"

lookupSucc
  ∷ ∀ a
  . VariantRep a
  → L.List String
  → L.List (BoundedDict a)
  → L.List (BoundedEnumDict a)
  → Maybe (VariantRep a)
lookupSucc (VariantRep rep) = go
  where
  go = case _, _, _ of
    L.Cons t1 ts1, L.Cons _ bs1, L.Cons d1 ds1
      | t1 == rep.type →
          case d1.succ rep.value of
            Just z  → Just $ VariantRep { type: t1, value: z }
            Nothing → case ts1, bs1 of
              L.Cons t2 _, L.Cons b2 _ → Just $ VariantRep { type: t2, value: b2.bottom }
              _, _ → Nothing
      | otherwise → go ts1 bs1 ds1
    _, _, _ → impossible "succ"

lookupCardinality
  ∷ ∀ a
  . L.List (BoundedEnumDict a)
  → Int
lookupCardinality = go 0
  where
  go acc = case _ of
    L.Cons d ds → go (acc + d.cardinality) ds
    L.Nil → acc

lookupFromEnum
  ∷ ∀ a
  . VariantRep a
  → L.List String
  → L.List (BoundedEnumDict a)
  → Int
lookupFromEnum (VariantRep rep) = go 0
  where
  go acc = case _, _ of
    L.Cons t ts, L.Cons d ds
      | t == rep.type → acc + d.fromEnum rep.value
      | otherwise → go (acc + d.cardinality) ts ds
    _, _ → impossible "fromEnum"

lookupToEnum
  ∷ ∀ a
  . Int
  → L.List String
  → L.List (BoundedEnumDict a)
  → Maybe (VariantRep a)
lookupToEnum = go
  where
  go ix = case _, _ of
    L.Cons t ts, L.Cons d ds
      | d.cardinality > ix →
          case d.toEnum ix of
            Just a → Just $ VariantRep { type: t, value: a }
            _ → Nothing
      | otherwise → go (ix - d.cardinality) ts ds
    _, _ → Nothing

class Contractable :: forall k. Row k -> Row k -> Constraint
class Contractable gt lt where
  contractWith ∷ ∀ proxy1 proxy2 f a. Alternative f ⇒ proxy1 gt → proxy2 lt → String → a → f a

instance contractWithInstance
  ∷ ( RL.RowToList lt ltl
    , R.Union lt a gt
    , VariantTags ltl
    )
  ⇒ Contractable gt lt
  where
  contractWith _ _ tag a
    | lookupTag tag (variantTags (Proxy ∷ Proxy ltl)) = pure a
    | otherwise = empty

type BoundedDict a =
  { top ∷ a
  , bottom ∷ a
  }

type BoundedEnumDict a =
  { pred ∷ a → M.Maybe a
  , succ ∷ a → M.Maybe a
  , fromEnum ∷ a → Int
  , toEnum ∷ Int → M.Maybe a
  , cardinality ∷ Int
  }

impossible ∷ ∀ a. String → a
impossible str = unsafeCrashWith $ "Data.Variant: impossible `" <> str <> "`"
