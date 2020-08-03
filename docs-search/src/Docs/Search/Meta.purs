module Docs.Search.Meta where

import Docs.Search.Config (config)
import Docs.Search.Loader as Loader
import Docs.Search.Types (GlobalIdentifier(..), PackageName)

import Prelude

import Effect.Aff (Aff, catchError)


type Meta =
  { localPackageName :: PackageName
  }


load :: Aff Meta
load =
  Loader.load (GlobalIdentifier config.metaItem) config.metaLoadPath
    `catchError` const (pure defaultMeta)
  where defaultMeta = { localPackageName: config.defaultPackageName }
