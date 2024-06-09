module Docs.Search.Meta where

import Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Docs.Search.Config as Config
import Docs.Search.Loader as Loader
import Docs.Search.Types (PackageName)
import Docs.Search.Types as Package
import Effect.Aff (Aff, catchError)

type Meta =
  { localPackageName :: PackageName
  }

metaCodec :: CJ.Codec Meta
metaCodec = CJ.named "Meta" $
  CJ.Record.object
    { localPackageName: Package.packageNameCodec
    }

load :: Aff Meta
load =
  Loader.load metaCodec Config.metaItem Config.metaLoadPath
    `catchError` const (pure defaultMeta)
  where
  defaultMeta = { localPackageName: Config.defaultPackageName }
