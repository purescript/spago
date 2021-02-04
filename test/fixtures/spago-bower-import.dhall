{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "simple-json"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "exceptions"
  , "foreign"
  , "foreign-object"
  , "generics-rep"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "record"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
