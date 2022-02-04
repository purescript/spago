let otherDependencies = [ "console" ]
in
{ name = "aaa"
, dependencies =
    otherDependencies # [ "effect", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
