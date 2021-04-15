let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f

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
            "v6.1.0"
      , halogen-css =
          mkPackage
            [ "halogen" ]
            "https://github.com/slamdata/purescript-halogen-css.git"
            "v8.0.0"
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
            "https://github.com/srghma/purescript-optparse.git"
            "d49b03fcd35f5be167e9c5c44ab1c17ca0956fb1"
      , exitcodes =
          mkPackage
            [ "enums" ]
            "https://github.com/Risto-Stevcev/purescript-exitcodes.git"
            "v4.0.0"
      , markdown-it =
          mkPackage
            [ "prelude", "effect", "options" ]
            "https://github.com/nonbili/purescript-markdown-it.git"
            "v0.4.0"
      , html-parser-halogen =
          mkPackage
            [ "string-parsers", "halogen" ]
            "https://github.com/rnons/purescript-html-parser-halogen.git"
            "458e492e441fcf69a66911b7b64beea5849e0dad"
      , markdown-it-halogen =
          mkPackage
            [ "markdown-it", "html-parser-halogen" ]
            "https://github.com/nonbili/purescript-markdown-it-halogen.git"
            "08c9625015bf04214be14e45230e8ce12f3fa2bf"
      , toppokki =
          mkPackage
            [ "prelude"
            , "record"
            , "functions"
            , "node-http"
            , "aff-promise"
            , "node-buffer"
            , "node-fs-aff"
            ]
            "https://github.com/justinwoo/purescript-toppokki.git"
            "v2.4.0"
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
            "5c1a44ee95c259352a2b4570b060de14130540bc"
      }

in  upstream // overrides // additions
