{ name = "my-project"
, dependencies = [ "console", "effect", "newtype", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, unexposedKey1 = 1
, unexposedKey2 = 2
}.{ name, dependencies, packages, sources }
