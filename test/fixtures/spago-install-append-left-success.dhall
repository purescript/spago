let otherDependencies = [ "console" ]

in  { name = "aaa"
    , dependencies =
        [ "arrays", "effect", "prelude", "psci-support" ] # otherDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
