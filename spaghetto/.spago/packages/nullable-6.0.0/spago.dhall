{ name = "nullable"
, dependencies =
  [ "assert"
  , "effect"
  , "functions"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
