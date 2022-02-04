let otherDependencies = [ "console" ]

in  { name = "aaa"
    , dependencies =
        otherDependencies # [ "arrays", "effect", "prelude", "psci-support" ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
