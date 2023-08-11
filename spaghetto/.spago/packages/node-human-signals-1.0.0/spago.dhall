{ name = "human-signals"
, dependencies =
  [ "arrays"
  , "control"
  , "foreign-object"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
