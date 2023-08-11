{ name = "js-uri"
, license = "MIT"
, repository = "https://github.com/purescript-contrib/purescript-js-uri"
, dependencies =
  [ "assert", "effect", "functions", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
