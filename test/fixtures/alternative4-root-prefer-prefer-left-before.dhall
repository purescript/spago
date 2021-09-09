{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [] : List Text
}
// { sources = [ "src/**/*.purs", "test/**/*.purs" ] }
//  { name = "my-project" }
