{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "docs-search"
, dependencies =
    [ "aff-promise"
    , "argonaut-codecs"
    , "argonaut-core"
    , "argonaut-generic"
    , "arrays"
    , "bower-json"
    , "console"
    , "control"
    , "coroutines"
    , "effect"
    , "foldable-traversable"
    , "generics-rep"
    , "halogen"
    , "halogen-css"
    , "lists"
    , "markdown-it"
    , "markdown-it-halogen"
    , "maybe"
    , "newtype"
    , "node-buffer"
    , "node-fs"
    , "node-fs-aff"
    , "node-process"
    , "node-readline"
    , "optparse"
    , "profunctor"
    , "search-trie"
    , "string-parsers"
    , "strings"
    , "test-unit"
    , "web-dom"
    , "web-html"
    ]
, packages =
    ./packages.dhall
}
