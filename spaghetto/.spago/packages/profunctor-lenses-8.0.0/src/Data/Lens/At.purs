module Data.Lens.At
  ( class At
  , at
  , sans
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Lens (Lens', lens, set)
import Data.Lens.Index (class Index)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Newtype (unwrap)
import Data.Set as S
import Foreign.Object as FO

-- | `At` is a type class whose instances let you add
-- | new elements or delete old ones from "container-like" types:
-- |
-- | ```purescript
-- | whole = Map.singleton "key" "value"
-- | optic = at "key"
-- |
-- | view optic whole == Just "value"
-- |
-- | set optic (Just "NEW") whole == Map.singleton "key" "NEW"
-- |
-- | set optic Nothing whole == Map.empty
-- | ```
-- |
-- | If you don't want to add or delete, but only to view or change
-- | an existing element, see `Data.Lens.Index`.

class Index m a b <= At m a b | m -> a, m -> b where
  at :: a -> Lens' m (Maybe b)

instance atIdentity :: At (Identity a) Unit a where
  at _ = lens (Just <<< unwrap) (flip maybe Identity)

instance atMaybe :: At (Maybe a) Unit a where
  at _ = lens identity \_ -> identity

instance atSet :: Ord v => At (S.Set v) v Unit where
  at x = lens get (flip update)
    where
    get xs =
      if S.member x xs then Just unit
      else Nothing
    update Nothing = S.delete x
    update (Just _) = S.insert x

instance atMap :: Ord k => At (M.Map k v) k v where
  at k =
    lens (M.lookup k) \m ->
      maybe' (\_ -> M.delete k m) \v -> M.insert k v m

instance atForeignObject :: At (FO.Object v) String v where
  at k =
    lens (FO.lookup k) \m ->
      maybe' (\_ -> FO.delete k m) \v -> FO.insert k v m

sans :: forall m a b. At m a b => a -> m -> m
sans k = at k `set` Nothing
