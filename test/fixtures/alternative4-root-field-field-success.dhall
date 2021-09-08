{ inner.config
  =
  { name = "my-project"
  , dependencies = [ "console", "effect", "newtype", "prelude", "psci-support" ]
  , packages = ./packages.dhall
  , sources = [ "src/**/*.purs", "test/**/*.purs" ]
  }
, other = "some other irrelevant value"
}.inner.config
