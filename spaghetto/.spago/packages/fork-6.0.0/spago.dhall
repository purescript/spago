{ name = "fork"
, dependencies = [ "aff", "console", "effect", "prelude", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
