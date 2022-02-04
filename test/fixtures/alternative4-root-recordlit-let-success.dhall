{ name = "my-project"
, dependencies =
    let x = [ "console", "effect", "prelude", "psci-support" ]

    in  x # [ "newtype" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
