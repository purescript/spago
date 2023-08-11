{ name =
    "b64"
, license =
    "Apache-2.0"
, repository =
    "git://github.com/menelaos/purescript-b64.git"
, dependencies =
    [ "arraybuffer-types"
    , "arrays"
    , "assert"
    , "console"
    , "effect"
    , "either"
    , "encoding"
    , "enums"
    , "exceptions"
    , "functions"
    , "partial"
    , "prelude"
    , "quickcheck"
    , "strings"
    , "stringutils"
    , "unicode"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
