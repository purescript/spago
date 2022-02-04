{ outer =
  { inner =
    { config =
      { name = "my-project"
      , dependencies = [] : List Text
      , packages = ./packages.dhall
      , sources = [ "src/**/*.purs", "test/**/*.purs" ]
      }
    }
  } with inner.config.dependencies = [ "console", "effect", "prelude", "psci-support" ]
}.outer.inner.config