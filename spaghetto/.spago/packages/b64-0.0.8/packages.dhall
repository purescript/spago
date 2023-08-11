let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220502/packages.dhall
        sha256:38d347aeba9fe6359c208abe87a5cecf1ffb14294f11ad19664ae35c59b6e29a

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
