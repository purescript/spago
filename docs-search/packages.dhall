let mkPackage =
	  https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190602/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
	  https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190602/src/packages.dhall sha256:5da1578dd297709265715a92eda5f42989dce92e121fcc889cff669a3b997c3d

let overrides = {=}

let additions =
	  { search-trie =
		  mkPackage
		  [ "prelude"
		  , "arrays"
		  , "ordered-collections"
		  , "lists"
		  , "foldable-traversable"
		  ]
		  "https://github.com/klntsky/purescript-search-trie.git"
		  "fd37a12"
	  , halogen =
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
		  "v5.0.0-rc.5"
	  , halogen-css =
		  mkPackage
		  [ "css", "halogen" ]
		  "https://github.com/slamdata/purescript-halogen-css.git"
		  "v8.0.0"
	  }

in  upstream ⫽ overrides ⫽ additions
