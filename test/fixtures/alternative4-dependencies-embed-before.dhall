{ name = "my-project"
, dependencies = ./embed-dependencies.dhall
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
