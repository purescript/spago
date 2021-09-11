{ name = "my-project"
, dependencies = [ "console", "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
