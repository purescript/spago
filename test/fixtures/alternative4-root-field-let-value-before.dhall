{ config =
  let x =
    { name = "my-project"
    , dependencies = [ "console", "effect", "prelude", "psci-support" ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
  in x
}.config
