let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190920/packages.dhall sha256:53873cf2fc4a343a41f335ee47c1706ecf755ac7c5a336e8eb03ad23165dfd28

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
