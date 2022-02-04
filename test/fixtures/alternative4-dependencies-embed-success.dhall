{ name = "my-project"
, dependencies = ./embed-dependencies.dhall # [ "newtype" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
