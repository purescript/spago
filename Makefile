all: fetch-templates

fetch-templates:
	@wget https://github.com/purescript/purescript-docs-search/releases/download/v0.0.10/docs-search-app.js -O templates/docs-search-app-0.0.10.js
	@wget https://github.com/purescript/purescript-docs-search/releases/download/v0.0.10/purescript-docs-search -O templates/purescript-docs-search-0.0.10
	@chmod +x templates/purescript-docs-search-0.0.10
	@wget https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/docs-search-app.js -O templates/docs-search-app-0.0.11.js
	@wget https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/purescript-docs-search -O templates/purescript-docs-search-0.0.11
	@chmod +x templates/purescript-docs-search-0.0.11