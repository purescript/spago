{ config =
  { name = "my-project"
  , dependencies = [] : List Text
  , packages = ./packages.dhall
  , sources = [ "src/**/*.purs", "test/**/*.purs" ]
  } // { dependencies = [ "console", "effect", "prelude", "psci-support" ] }
}.config