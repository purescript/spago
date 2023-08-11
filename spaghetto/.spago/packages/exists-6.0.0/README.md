# purescript-exists

[![Latest release](http://img.shields.io/github/release/purescript/purescript-exists.svg)](https://github.com/purescript/purescript-exists/releases)
[![Build status](https://github.com/purescript/purescript-exists/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-exists/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-exists/badge)](https://pursuit.purescript.org/packages/purescript-exists)

The `Exists` type, for encoding existential types.

## Installation

```
spago install exists
```

## Overview

The type `Exists f` is isomorphic to the existential type `exists a. f a`.

For example, consider the type `exists s. Tuple s (s -> Tuple s a)` which represents infinite streams of elements of type `a`.

This type can be constructed by creating a type constructor `StreamF` as follows:

```purescript
data StreamF a s = StreamF s (s -> Tuple s a)
```

We can then define the type of streams using `Exists`:

```purescript
type Stream a = Exists (StreamF a)
```

The `mkExists` and `runExists` functions then enable packing and unpacking of `SteamF` into/out of `Stream`.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-exists).
