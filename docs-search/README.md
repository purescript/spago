# purescript-docs-search

An app that adds search capabilities to generated documentation for purescript code.

The goal is to replicate all functionality of pursuit, including querying by type.

See [#89](https://github.com/spacchetti/spago/issues/89).

To see it in action, run the following:

```
spago build
spago docs
spago bundle-app -m Docs.Search.App --to generated-docs/docs-search-app.js
spago run -m Docs.Search.IndexBuilder
```
