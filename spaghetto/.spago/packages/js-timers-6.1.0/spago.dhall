{ name = "js-timers"
, dependencies =
  [ "assert", "console", "effect", "prelude", "refs" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
