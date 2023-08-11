{ name = "spec"
, repository = "https://github.com/purescript-spec/purescript-spec.git"
, license = "MIT"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "fork"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "parallel"
  , "pipes"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
