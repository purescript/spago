{ name = "affjax-node"
, dependencies = [ "aff", "affjax", "argonaut-core", "datetime", "effect", "either", "exceptions", "foreign-object", "http-methods", "maybe", "transformers", "console", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
