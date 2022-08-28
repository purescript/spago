{ name = "spaghetto"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
