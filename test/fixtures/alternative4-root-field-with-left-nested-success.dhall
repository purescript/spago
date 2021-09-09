{ outer =
    { inner.config
      =
      { name = "my-project"
      , dependencies =
        [ "console", "effect", "newtype", "prelude", "psci-support" ]
      , packages = ./packages.dhall
      , sources = [] : List Text
      }
    }
  with inner.config.sources = [ "src/**/*.purs", "test/**/*.purs" ]
}.outer.inner.config
