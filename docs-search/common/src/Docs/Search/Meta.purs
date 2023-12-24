module Docs.Search.Meta where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Docs.Search.Config as Config
import Docs.Search.Loader as Loader
import Docs.Search.Types (PackageName)
import Docs.Search.Types as Package
import Effect.Aff (Aff, catchError)

type Meta =
  { localPackageName :: PackageName
  }

metaCodec :: JsonCodec Meta
metaCodec =
  CAR.object "Meta"
    { localPackageName: Package.packageNameCodec
    }

load :: Aff Meta
load =
  Loader.load metaCodec Config.metaItem Config.metaLoadPath
    `catchError` const (pure defaultMeta)
  where
  defaultMeta = { localPackageName: Config.defaultPackageName }
