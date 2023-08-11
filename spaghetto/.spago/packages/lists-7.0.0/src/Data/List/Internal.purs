module Data.List.Internal (Set, emptySet, insertAndLookupBy) where

import Prelude

import Data.List.Types (List(..))

data Set k
  = Leaf
  | Two (Set k) k (Set k)
  | Three (Set k) k (Set k) k (Set k)

emptySet :: forall k. Set k
emptySet = Leaf

data TreeContext k
  = TwoLeft k (Set k)
  | TwoRight (Set k) k
  | ThreeLeft k (Set k) k (Set k)
  | ThreeMiddle (Set k) k k (Set k)
  | ThreeRight (Set k) k (Set k) k

fromZipper :: forall k. List (TreeContext k) -> Set k -> Set k
fromZipper Nil tree = tree
fromZipper (Cons x ctx) tree =
  case x of
    TwoLeft k1 right -> fromZipper ctx (Two tree k1 right)
    TwoRight left k1 -> fromZipper ctx (Two left k1 tree)
    ThreeLeft k1 mid k2 right -> fromZipper ctx (Three tree k1 mid k2 right)
    ThreeMiddle left k1 k2 right -> fromZipper ctx (Three left k1 tree k2 right)
    ThreeRight left k1 mid k2 -> fromZipper ctx (Three left k1 mid k2 tree)

data KickUp k = KickUp (Set k) k (Set k)

-- | Insert or replace a key/value pair in a map
insertAndLookupBy :: forall k. (k -> k -> Ordering) -> k -> Set k -> { found :: Boolean, result :: Set k }
insertAndLookupBy comp k orig = down Nil orig
  where
  down :: List (TreeContext k) -> Set k -> { found :: Boolean, result :: Set k }
  down ctx Leaf = { found: false, result: up ctx (KickUp Leaf k Leaf) }
  down ctx (Two left k1 right) =
    case comp k k1 of
      EQ -> { found: true, result: orig }
      LT -> down (Cons (TwoLeft k1 right) ctx) left
      _  -> down (Cons (TwoRight left k1) ctx) right
  down ctx (Three left k1 mid k2 right) =
    case comp k k1 of
      EQ -> { found: true, result: orig }
      c1 ->
        case c1, comp k k2 of
          _ , EQ -> { found: true, result: orig }
          LT, _  -> down (Cons (ThreeLeft k1 mid k2 right) ctx) left
          GT, LT -> down (Cons (ThreeMiddle left k1 k2 right) ctx) mid
          _ , _  -> down (Cons (ThreeRight left k1 mid k2) ctx) right

  up :: List (TreeContext k) -> KickUp k -> Set k
  up Nil (KickUp left k' right) = Two left k' right
  up (Cons x ctx) kup =
    case x, kup of
      TwoLeft k1 right, KickUp left k' mid -> fromZipper ctx (Three left k' mid k1 right)
      TwoRight left k1, KickUp mid k' right -> fromZipper ctx (Three left k1 mid k' right)
      ThreeLeft k1 c k2 d, KickUp a k' b -> up ctx (KickUp (Two a k' b) k1 (Two c k2 d))
      ThreeMiddle a k1 k2 d, KickUp b k' c -> up ctx (KickUp (Two a k1 b) k' (Two c k2 d))
      ThreeRight a k1 b k2, KickUp c k' d -> up ctx (KickUp (Two a k1 b) k2 (Two c k' d))
