{-
 Welcome to Spacchetti local packages!
You can edit this file as you like.

e.g.

```
in  let overrides =
        { halogen =
        upstream.halogen ⫽ { version = "master" }
        , halogen-vdom =
        upstream.halogen-vdom ⫽ { version = "v4.0.0" }
        }
```
-}

    let mkPackage =
          https://raw.githubusercontent.com/justinwoo/spacchetti/241018/src/mkPackage.dhall

in  let upstream =
          https://raw.githubusercontent.com/justinwoo/spacchetti/241018/src/packages.dhall

in  let overrides = {=}

in  let additions = {=}

in  upstream ⫽ overrides ⫽ additions
