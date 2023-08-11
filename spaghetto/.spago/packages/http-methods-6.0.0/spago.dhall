{ name = "http-methods"
, dependencies =
  [ "console", "effect", "either", "prelude", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
