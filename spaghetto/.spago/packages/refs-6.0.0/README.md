# purescript-refs

[![Latest release](http://img.shields.io/github/release/purescript/purescript-refs.svg)](https://github.com/purescript/purescript-refs/releases)
[![Build status](https://github.com/purescript/purescript-refs/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-refs/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-refs/badge)](https://pursuit.purescript.org/packages/purescript-refs)

This module defines functions for working with mutable value references.

_Note_: [`Control.Monad.ST`](https://pursuit.purescript.org/packages/purescript-st/4.0.0/docs/Control.Monad.ST) provides a _safe_ alternative to `Ref` when mutation is restricted to a local scope.

## Installation

```
spago install refs
```

## Example

```purs
import Effect.Ref as Ref

main = do
  -- initialize a new Ref with the value 0
  ref <- Ref.new 0

  -- read from it and check it
  curr1 <- Ref.read ref
  assertEqual { actual: curr1, expected: 0 }

  -- write over the ref with 1
  Ref.write 1 ref

  -- now it is 1 when we read out the value
  curr2 <- Ref.read ref
  assertEqual { actual: curr2, expected: 1 }

  -- modify it by adding 1 to the current state
  Ref.modify_ (\s -> s + 1) ref

  -- now it is 2 when we read out the value
  curr3 <- Ref.read ref
  assertEqual { actual: curr3, expected: 2 }
```

See [tests](test/Main.purs) to see usages.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-refs).
