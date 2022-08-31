let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220829/packages.dhall
        sha256:9ec64ed24ec308770ccdd3d0edb74a814fcfbe34c7df5c28266935f0b5e585d7

let overrides = {=}

let additions =
      { halogen =
          mkPackage
            [ "aff"
            , "avar"
            , "console"
            , "const"
            , "dom-indexed"
            , "effect"
            , "foreign"
            , "fork"
            , "free"
            , "freeap"
            , "halogen-subscriptions"
            , "halogen-vdom"
            , "media-types"
            , "nullable"
            , "ordered-collections"
            , "parallel"
            , "profunctor"
            , "transformers"
            , "unsafe-coerce"
            , "unsafe-reference"
            , "web-file"
            , "web-uievents"
            ]
            "https://github.com/purescript-halogen/purescript-halogen.git"
            "v7.0.0"
      , memoize =
          mkPackage
            [ "prelude"
            , "lazy"
            , "either"
            , "maybe"
            , "tuples"
            , "integers"
            , "lists"
            , "strings"
            ]
            "https://github.com/paf31/purescript-memoize.git"
            "9960694e82adc212fd89f8ed8778cf55fcb72aeb"
      , optparse =
          mkPackage
            [ "prelude"
            , "effect"
            , "exitcodes"
            , "strings"
            , "ordered-collections"
            , "arrays"
            , "console"
            , "transformers"
            , "exists"
            , "node-process"
            , "free"
            , "memoize"
            ]
            "https://github.com/klntsky/purescript-optparse.git"
            "2fe4265b7c6b09744c11190a43ca06777c752473"
      , exitcodes =
          mkPackage
            [ "enums" ]
            "https://github.com/Risto-Stevcev/purescript-exitcodes.git"
            "v4.0.0"
      , markdown-it =
          mkPackage
            [ "prelude", "effect", "options" ]
            "https://github.com/klntsky/purescript-markdown-it.git"
            "f3b7654783a83a80d7c09b6caaa7cd40b93ddce1"
      , string-parsers =
          mkPackage
            [ "arrays"
            , "assert"
            , "bifunctors"
            , "console"
            , "control"
            , "effect"
            , "either"
            , "enums"
            , "foldable-traversable"
            , "lists"
            , "maybe"
            , "minibench"
            , "nonempty"
            , "partial"
            , "prelude"
            , "strings"
            , "tailrec"
            , "transformers"
            , "unfoldable"
            ]
            "https://github.com/purescript-contrib/purescript-string-parsers.git"
            "v8.0.0"
      , html-parser-halogen =
          mkPackage
            [ "string-parsers", "halogen" ]
            "https://github.com/rnons/purescript-html-parser-halogen.git"
            "035a51d02ba9f8b70c3ffd9fe31a3f5bed19941c"
      , markdown-it-halogen =
          mkPackage
            [ "markdown-it", "html-parser-halogen" ]
            "https://github.com/nonbili/purescript-markdown-it-halogen.git"
            "08c9625015bf04214be14e45230e8ce12f3fa2bf"
      , search-trie =
          mkPackage
            [ "prelude"
            , "arrays"
            , "ordered-collections"
            , "lists"
            , "foldable-traversable"
            , "bifunctors"
            ]
            "https://github.com/klntsky/purescript-search-trie.git"
            "e7f7f22486a1dba22171ec885dbc2149dc815119"
      , css =
          mkPackage
            [ "colors"
            , "console"
            , "effect"
            , "exceptions"
            , "nonempty"
            , "profunctor"
            , "psci-support"
            , "strings"
            , "these"
            , "transformers"
            ]
            "https://github.com/purescript-contrib/purescript-css.git"
            "710d6a742beb88299faf08aaeb997ee1e24483ab"
      , jest =
          mkPackage
            [ "aff"
            , "aff-promise"
            , "effect"
            , "prelude"
            , "psci-support"
            , "foldable-traversable"
            ]
            "https://github.com/klntsky/purescript-jest.git"
            "7feaa5a880fc75002c4eca312993174e7220252b"
      , literals =
          mkPackage
            [ "assert"
            , "effect"
            , "console"
            , "integers"
            , "numbers"
            , "partial"
            , "psci-support"
            , "unsafe-coerce"
            , "typelevel-prelude"
            ]
            "https://github.com/jvliwanag/purescript-literals.git"
            "ae3ef4e9c1ae7c57ec77bd13906fa60ae8abba4a"
      , untagged-union =
          mkPackage
            [ "assert"
            , "console"
            , "effect"
            , "foreign"
            , "foreign-object"
            , "literals"
            , "maybe"
            , "newtype"
            , "psci-support"
            , "tuples"
            , "unsafe-coerce"
            ]
            "https://github.com/jvliwanag/purescript-untagged-union.git"
            "ed8262a966e15e751322c327e2759a9b9c0ef3f3"
      }

in  upstream // overrides // additions
