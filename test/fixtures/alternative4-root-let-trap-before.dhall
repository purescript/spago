let listBinding = [ "console", "effect", "prelude", "psci-support" ]

in  { name = "my-project"
    , dependencies = let x = listBinding let y = x let z = y in z
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
