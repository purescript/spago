module Data.Lens.Fold.Partial
  ( unsafeView
  , (^?!)
  , unsafeIndexedFold
  , (^@?!)
  ) where

import Prelude

import Data.Lens.Fold (Fold, ifoldMapOf, previewOn)
import Data.Lens.Types (IndexedFold)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

unsafeView :: forall s t a b. Partial => s -> Fold (First a) s t a b -> a
unsafeView s l = fromMaybe' (\_ -> unsafeCrashWith "unsafeView: Empty fold") $ previewOn s l

infixl 8 unsafeView as ^?!

unsafeIndexedFold
  :: forall i s t a b
   . Partial
  => s
  -> IndexedFold (First (Tuple i a)) i s t a b
  -> Tuple i a
unsafeIndexedFold s l = fromMaybe' (\_ -> unsafeCrashWith "unsafeIndexedFold: empty Fold")
  $ unwrap
  $ ifoldMapOf l (\i a -> First $ Just (Tuple i a)) s

infixl 8 unsafeIndexedFold as ^@?!
