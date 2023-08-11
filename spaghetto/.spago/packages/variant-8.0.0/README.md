# purescript-variant

[![Latest release](http://img.shields.io/github/release/natefaubion/purescript-variant.svg)](https://github.com/natefaubion/purescript-variant/releases)
[![Build status](https://travis-ci.org/natefaubion/purescript-variant.svg?branch=master)](https://travis-ci.org/natefaubion/purescript-variant)

Polymorphic variants for PureScript.

## Install

```
bower install purescript-variant
```

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-variant).

`Data.Variant` is an implementation of polymorphic variants in PureScript. What
are polymorphic variants? Before we get to that, lets look at the dual, which
you are likely familiar with if you've been using PureScript: records.

Another name for records might be polymorphic products. A product is simply
data that holds inhabitants for more than one type at a time, `Tuple a b` being
the canonical product.

```purescript
data Tuple a b = Tuple a b
```

If I have a `Tuple Int String`, then I have available some `Int` value paired
with a `String` value (or `Int * String`, thus a product). For convenience,
we often like to use records, especially for models in our shiny web apps.

```purescript
type User =
  { name :: String
  , age :: Int
  , email :: Email
  }
```

And maybe use it like so:

```purescript
addJrSuffix :: User -> User
addJrSuffix user = user { name = user.name <> ", Jr." }
```

However this type signature is needlessly specific. In fact, all it cares about is
the `name` field. We can express this sort of structural typing in PureScript
via row types:

```purescript
addJrSuffix :: forall r. { name :: String | r } -> { name :: String | r }
addJrSuffix hasName = hasName { name = hasName.name <> ", Jr." }
```

Now I can pass in anything that merely has a `name :: String`.

```purescript
addJrSuffix { name: "Bob" }
addJrSuffix { age: 42, name: "Gerald" }
```

So records, or polymorphic products, let us pass in anything as long as it has
the structure we specify, and even get the same structure back after we are
done with it.

Let's flip back around to sum types (or variants), `Either a b` being the
canonical dual of `Tuple a b`.

```purescript
data Either a b = Left a | Right b
```

Where `Tuple a b` says we have an `a` paired with a `b`, `Either a b` says we
have either an `a` _or_ a `b` via the `Left` and `Right` constructors. We'd
handle the possibilities by pattern matching on it with `case`.

This library just uses the same structural row system that we use with records
(products) and applies them to variants (sums). Voila!

We lift values into `Variant` with `inj` by specifying a _tag_.

```purescript
import Type.Proxy (Proxy(..))

someFoo :: forall v. Variant (foo :: Int | v)
someFoo = inj (Proxy :: Proxy "foo") 42
```

`Proxy` is just a way to tell the compiler what our tag is at the type level.
I can stamp out a bunch of these with different labels:

```purescript
someFoo :: forall v. Variant (foo :: Int | v)
someFoo = inj (Proxy :: Proxy "foo") 42

someBar :: forall v. Variant (bar :: Boolean | v)
someBar = inj (Proxy :: Proxy "bar") true

someBaz :: forall v. Variant (baz :: String | v)
someBaz = inj (Proxy :: Proxy "baz") "Baz"
```

We can try to extract a value from this via `on`, which takes a function to
handle the inner value in case of success, and a function to handle the rest in
case of failure.

```purescript
fooToString :: forall v. Variant (foo :: Int | v) -> String
fooToString = on (Proxy :: Proxy "foo") show (\_ -> "not foo")

fooToString someFoo == "42"
fooToString someBar == "not foo"
```

We can chain usages of `on` and terminate it with `case_` (for compiler-checked
exhaustivity) or `default` (to provide a default value in case of failure).

```purescript
_foo = Proxy :: Proxy "foo"
_bar = Proxy :: Proxy "bar"
_baz = Proxy :: Proxy "baz"

allToString :: Variant (foo :: Int, bar :: Boolean, baz :: String) -> String
allToString =
  case_
    # on _foo show
    # on _bar (if _ then "true" else "false")
    # on _baz (\str -> str)

someToString :: forall v. Variant (foo :: Int, bar :: Boolean | v) -> String
someToString =
  default "unknown"
    # on _foo show
    # on _bar (if _ then "true" else "false")

allToString someBaz == "Baz"
someToString someBaz == "unknown"
```

Handlers with `on` are also compositional! We can compose them together with
function composition and reuse them in different contexts.

```purescript
onFooOrBar :: forall v. (Variant v -> String) -> Variant (foo :: Int, bar :: Boolean | v) -> String
onFooOrBar = on _foo show >>> on _bar (if _ then "true" else "false")

allToString :: Variant (foo :: Int, bar :: Boolean, baz :: String) -> String
allToString =
  case_
    # onFooOrBar
    # on _baz (\str -> str)
```

Instead of chaining with just `on`, there is `onMatch` which adds record sugar.

```purescript
onFooOrBar :: forall v. (Variant v -> String) -> Variant (foo :: Int, bar :: Boolean | v) -> String
onFooOrBar = onMatch
  { foo: show :: Int -> String
  , bar: if _ then "true" else "false"
  }
```

But note that polymorphic functions like `show` or `id` need to be either
annotated or eta expanded due to record impredicativity.

`onMatch` can be used with `case_` and `default` just like `on`, but there is
also `match` for the common case of total matching.

```purescript
allToString :: Variant (foo :: Int, bar :: Boolean, baz :: String) -> String
allToString = match
  { foo: \a -> show a
  , bar: \a -> if a then "true" else "false"
  , baz: \a -> a
  }
```

We can combine polymorphic variants with `Functor`s as well using `VariantF`,
which lives in `Data.Functor.Variant`. `VariantF` is just like `Variant`,
except it's indexed by things of kind `Type -> Type`.

```purescript
someFoo :: forall v. VariantF (foo :: Maybe | v) Int
someFoo = inj (Proxy :: Proxy "foo") (Just 42)

someBar :: forall v. VariantF (bar :: Tuple String | v) Int
someBar = inj (Proxy :: Proxy "bar") (Tuple "bar" 42)

someBaz :: forall v a. VariantF (baz :: Either String | v) a
someBaz = inj (Proxy :: Proxy "baz") (Left "Baz")
```

`VariantF` supports all the same combinators as `Variant`.
