{ name = "node-event-emitters"
, dependencies =
  [ "effect"
  , "functions"
  , "prelude"
  , "safe-coerce"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
