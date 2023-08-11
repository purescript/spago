{ name = "routing-duplex"
, dependencies =
  [ "arrays"
  , "assert"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "js-uri"
  , "lazy"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "record"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
