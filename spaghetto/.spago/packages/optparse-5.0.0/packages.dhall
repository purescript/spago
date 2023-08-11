let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220507/packages.dhall
        sha256:cf54330f3bc1b25a093b69bff8489180c954b43668c81288901a2ec29a08cc64
      with open-memoize =
        { dependencies =
          [ "console"
          , "effect"
          , "psci-support"
          , "strings"
          , "lists"
          , "either"
          , "integers"
          , "lazy"
          , "maybe"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo =
            "https://github.com/purescript-open-community/purescript-open-memoize.git"
        , version = "v6.1.0"
        }

in  upstream
