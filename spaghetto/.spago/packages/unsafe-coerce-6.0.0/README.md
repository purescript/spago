# purescript-unsafe-coerce

[![Latest release](http://img.shields.io/github/release/purescript/purescript-unsafe-coerce.svg)](https://github.com/purescript/purescript-unsafe-coerce/releases)
[![Build status](https://github.com/purescript/purescript-unsafe-coerce/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-unsafe-coerce/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-unsafe-coerce/badge)](https://pursuit.purescript.org/packages/purescript-unsafe-coerce)

A _highly unsafe_ function, which can be used to persuade the type system that any type is the same as any other type. When using this function, it is your (that is, the caller's) responsibility to ensure that the underlying representation for both types is the same.

There are few situations where it is acceptable to use this function, it should only ever appear as an internal implementation detail of a library, never as a function used in a "normal" codebase.

## Installation

```
spago install unsafe-coerce
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-unsafe-coerce).
