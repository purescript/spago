module Data.Argonaut.Gen where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core as J
import Data.Array as A
import Data.NonEmpty ((:|))
import Data.String.Gen (genUnicodeString)
import Foreign.Object as Obj

-- | A generator for `Json` values. Especially useful for writing property-based
-- | tests.
genJson :: forall m. MonadGen m => MonadRec m => Lazy (m J.Json) => m J.Json
genJson = Gen.resize (min 5) $ Gen.sized genJson'
  where
  genJson' :: Int -> m J.Json
  genJson' size
    | size > 1 = Gen.resize (_ - 1) (Gen.choose genJArray genJObject)
    | otherwise = genLeaf

  genLeaf :: m J.Json
  genLeaf = Gen.oneOf $ pure J.jsonNull :| [ genJBoolean, genJNumber, genJString ]

  genJArray :: m J.Json
  genJArray = J.fromArray <$> Gen.unfoldable (defer \_ -> genJson)

  genJObject :: m J.Json
  genJObject = A.foldM extendJObj J.jsonEmptyObject =<< Gen.unfoldable genUnicodeString

  extendJObj :: J.Json -> String -> m J.Json
  extendJObj obj k = do
    v <- genJson
    pure $
      J.caseJsonObject
        (J.jsonSingletonObject k v)
        (J.fromObject <<< Obj.insert k v)
        obj

  genJBoolean :: m J.Json
  genJBoolean = J.fromBoolean <$> Gen.chooseBool

  genJNumber :: m J.Json
  genJNumber = J.fromNumber <$> Gen.chooseFloat (-1000000.0) 1000000.0

  genJString :: m J.Json
  genJString = J.fromString <$> genUnicodeString
