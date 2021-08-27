{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
let main =
  { dependencies = [ "console", "effect", "prelude", "psci-support" ]
  , sources = [ "src/**/*.purs" ]
  }
let test =
  { dependencies = main.dependencies # [ "spec" ]
  , sources = main.sources # [ "test/**/*.purs" ]
  }

in
  { name = "my-project"
  , packages = ./packages.dhall
  , targets =
    { main = main
    , test = test
    }
  }
