{ name = "my-project"
, dependencies = [] : List Text
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
  with dependencies =
    [ "console", "effect", "newtype", "prelude", "psci-support" ]
