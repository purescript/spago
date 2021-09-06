let otherDependencies1 = [ "console" ]

let otherDependencies2 = [ "effect" ]

in  { name = "aaa"
    , dependencies =
          otherDependencies1
        # [ "arrays", "prelude", "psci-support" ]
        # otherDependencies2
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
