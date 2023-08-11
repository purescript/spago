{ name = "now"
, dependencies =
  [ "assert"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "node-process"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
