{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
// { dependencies = [ "console", "effect", "prelude", "psci-support" ] }
//  { name = "my-project" }
