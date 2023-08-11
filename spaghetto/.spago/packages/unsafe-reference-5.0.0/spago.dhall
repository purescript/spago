{ name = "unsafe-reference"
, dependencies =
  [ "console", "effect", "exceptions", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
