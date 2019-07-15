# purescript-docs-search

[![Build status](https://travis-ci.org/spacchetti/purescript-docs-search.svg?branch=master)](https://travis-ci.org/spacchetti/purescript-docs-search)

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

## UI

The user interface of the app is optimised for keyboard-only use.

**S** hotkey can be used to focus on the search field, **Escape** can be used to leave it. Pressing **Escape** twice will close the search results listing.
