module Docs.Search.Meta where

import Docs.Search.Config as Config
import Docs.Search.Loader as Loader
import Docs.Search.Types (PackageName)

import Prelude

import Effect.Aff (Aff, catchError)

type Meta =
  { localPackageName :: PackageName
  }

load :: Aff Meta
load =
  Loader.load Config.metaItem Config.metaLoadPath
    `catchError` const (pure defaultMeta)
  where
  defaultMeta = { localPackageName: Config.defaultPackageName }
