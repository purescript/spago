{ name =
	"my-project"
, dependencies =
	[ "effect"
	, "aff-promise"
	, "argonaut-codecs"
	, "argonaut-core"
	, "arrays"
	, "console"
	, "foldable-traversable"
	, "generics-rep"
	, "lists"
	, "maybe"
	, "newtype"
	, "node-buffer"
	, "node-fs"
	, "node-fs-aff"
	, "node-process"
	, "psci-support"
	, "search-trie"
	, "strings"
	, "web-dom"
	, "web-html"
	, "halogen"
	, "halogen-css"
	, "coroutines"
	, "test-unit"
	]
, packages =
	./packages.dhall
}
