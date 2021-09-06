let otherDependencies = [ "console" ]
in
{ name = "aaa"
, dependencies =
    [ "effect", "prelude", "psci-support" ] # otherDependencies
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
