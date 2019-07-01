{ name =
	"my-project"
, dependencies =
	[ "aff-promise"
	, "argonaut-codecs"
	, "argonaut-core"
	, "argonaut-generic"
	, "arrays"
	, "console"
	, "control"
	, "coroutines"
	, "effect"
	, "foldable-traversable"
	, "generics-rep"
	, "halogen"
	, "halogen-css"
	, "lists"
	, "maybe"
	, "newtype"
	, "node-buffer"
	, "node-fs"
	, "node-fs-aff"
	, "node-process"
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
