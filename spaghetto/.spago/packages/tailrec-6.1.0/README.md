# purescript-tailrec

[![Latest release](http://img.shields.io/github/release/purescript/purescript-tailrec.svg)](https://github.com/purescript/purescript-tailrec/releases)
[![Build status](https://github.com/purescript/purescript-tailrec/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-tailrec/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-tailrec/badge)](https://pursuit.purescript.org/packages/purescript-tailrec)

A type class which captures stack-safe monadic tail recursion.

## Installation

```
spago install tailrec
```

## Usage

The PureScript compiler performs tail-call elimination for self-recursive functions, so that a function like

```purescript
pow :: Int -> Int -> Int
pow n p = go { accum: 1, power: p }
  where
  go { accum: acc, power: 0 } = acc
  go { accum: acc, power: p } = go { accum: acc * n, power: p - 1 }
```

gets compiled into an efficient `while` loop.

However, we do not get the same benefit when using monadic recursion:

```purescript
powWriter :: Int -> Int -> Writer Product Unit
powWriter n = go
  where
  go 0 = return unit
  go m = do
    tell n
    go (m - 1)
```

However, we can refactor the original function to isolate the recursive function call:

```purescript
pow :: Int -> Int -> Int
pow n p = tailRec go { accum: 1, power: p }
  where
  go :: _ -> Step _ Int
  go { accum: acc, power: 0 } = Done acc
  go { accum: acc, power: p } = Loop { accum: acc * n, power: p - 1 }
```

where the `tailRec` function is defined in the `Control.Monad.Rec.Class` module, with type:

```purescript
tailRec :: forall a b. (a -> Step a b) -> a -> b
```

In the body of the loop, instead of calling the `go` function recursively, we return a value using the `Loop` constructor. To break from the loop, we use the `Done` constructor.

This pattern can be generalized to several monad transformers from the `purescript-transformers` library using the following type class:

```purescript
class Monad m <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-tailrec).
