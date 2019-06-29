# spago-search

An app that adds search capabilities to generated documentation for purescript code.

The goal is to replicate all functionality of pursuit, including querying by type.

See [#89](https://github.com/spacchetti/spago/issues/89).



```
spago build
spago docs
spago bundle-app -m Spago.Search.App --to generated-docs/spago-search-app.js
spago run -m Spago.Search.IndexBuilder
```
