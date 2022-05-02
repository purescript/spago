{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "aaa"
, dependencies = [ "console", "effect", "foreign", "prelude", "either" ]
, packages = ./packages.dhall
}
