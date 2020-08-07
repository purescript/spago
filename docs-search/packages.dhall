let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200724/packages.dhall sha256:bb941d30820a49345a0e88937094d2b9983d939c9fd3a46969b85ce44953d7d9

let overrides = {=}

let additions =
      { halogen =
          mkPackage
            [ "aff"
            , "avar"
            , "console"
            , "const"
            , "coroutines"
            , "dom-indexed"
            , "foreign"
            , "fork"
            , "free"
            , "freeap"
            , "halogen-vdom"
            , "media-types"
            , "nullable"
            , "ordered-collections"
            , "parallel"
            , "profunctor"
            , "transformers"
            , "unsafe-coerce"
            , "unsafe-reference"
            , "web-uievents"
            ]
            "https://github.com/slamdata/purescript-halogen.git"
            "v5.0.0-rc.6"
      , halogen-css =
          mkPackage
            [ "css", "halogen" ]
            "https://github.com/slamdata/purescript-halogen-css.git"
            "v8.0.0"
      , optparse =
          mkPackage
            [ "prelude"
            , "effect"
            , "exitcodes"
            , "strings"
            , "ordered-collections"
            , "arrays"
            , "console"
            , "memoize"
            , "transformers"
            , "exists"
            , "node-process"
            , "free"
            ]
            "https://github.com/f-o-a-m/purescript-optparse.git"
            "v3.0.1"
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
            [ "string-parsers", "generics-rep", "halogen" ]
            "https://github.com/rnons/purescript-html-parser-halogen.git"
            "890da763cdd2a1049ab8837e477c5ba1fcf6d4ce"
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
      }

in  upstream // overrides // additions
