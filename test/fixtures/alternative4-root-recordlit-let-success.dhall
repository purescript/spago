{ name = "my-project"
, dependencies =
    let x = [ "console", "effect", "newtype", "prelude", "psci-support" ] in x
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
