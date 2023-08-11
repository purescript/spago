{ name = "fixed-points"
, dependencies =
  [ "control", "exists", "newtype", "prelude", "transformers", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
