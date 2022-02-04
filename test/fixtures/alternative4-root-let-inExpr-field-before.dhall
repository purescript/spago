let x = { dependencies = [ "console", "effect", "prelude", "psci-support" ] }
in
{ name = "my-project"
, dependencies = x.dependencies
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
