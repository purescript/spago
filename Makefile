all: fetch-templates

fetch-templates:
	@curl https://github.com/purescript/purescript-docs-search/releases/download/v0.0.10/docs-search-app.js -o templates/docs-search-app-0.0.10.js
	@curl https://github.com/purescript/purescript-docs-search/releases/download/v0.0.10/purescript-docs-search -o templates/purescript-docs-search-0.0.10
	@chmod +x templates/purescript-docs-search-0.0.10
	@curl https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/docs-search-app.js -o templates/docs-search-app-0.0.11.js
	@curl https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/purescript-docs-search -o templates/purescript-docs-search-0.0.11
	@chmod +x templates/purescript-docs-search-0.0.11

