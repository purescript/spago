# purescript-gen

[![Latest release](http://img.shields.io/github/release/purescript/purescript-gen.svg)](https://github.com/purescript/purescript-gen/releases)
[![Build status](https://github.com/purescript/purescript-gen/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-gen/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-gen/badge)](https://pursuit.purescript.org/packages/purescript-gen)

A type class for random generator implementations.

Note that this library only provides the `MonadGen` type class; it does not provide any implementations of it. The intention is that libraries may depend on `purescript-gen` in order to provide functions for random generation of any data types they define while also keeping their dependency footprints minimal, and not tying users to any specific random generation library (since any library implementing random generation of values should be able to implement the `MonadGen` interface).

## Installation

```
spago install gen
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-gen).
