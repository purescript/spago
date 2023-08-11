# purescript-convertable-options

PureScript semantics for highly-overloaded API interfaces.

* Options with implicit defaults.
* Options with conversions - feels a lot like untagged unions.
* Options with `Maybe` lifting - feels a lot like nullable fields.

## Introduction

Say we have an API:

```purescript
flub :: { foo :: Int, bar :: String, baz :: Maybe Boolean } -> String
```

This API has very straightforwad and understandable options.

```purescript
example = flub
  { foo: 42
  , bar: "Hello"
  , baz: Nothing
  }
```

But we find this inconvenient.
  * `foo` has an obvious default value.
  * `bar` is a `String`, but we also want to provide an `Int`.
  * `baz` is often `Nothing`, and we don't want to always have to wrap with `Just`.

That is, we'd like to call it in many different ways at our leisure:

```purescript
flub { bar: "Hello" }
flub { bar: 99, baz: true }
flub { foo: 12, bar: "OK", baz: Just false }
```

To start, we should separate out type declarations for defaulted (optional)
fields and all fields.

```purescript
type Optional =
  ( foo :: Int
  , baz :: Maybe Boolean
  )

type All =
  ( bar :: String
  | Optional
  )

defaultOptions :: { | Optional }
defaultOptions =
  { foo: 42
  , baz: Nothing
  }

flub :: { | All } -> String
```

If all we want is defaulting, we can use a `Defaults` constraint.

```purescript
flub
  :: forall provided
   . Defaults { | Optional } { | provided } { | All }
  => { | provided }
  -> String
flub provided = ...
  where
  all :: { | All }
  all = defaults defaultOptions provided
```

This will let us omit `foo` and `baz`:

```purescript
flub { bar: "Hello" }
flub { foo: 99, bar: "Hello" }
flub { foo: 99, bar: "Hello", baz: Just true }
```

However, we still must always wrap `baz` with `Just`, and we cannot provide
an `Int` for `bar`. To do that we must define `ConvertOption` instances.

To dispatch `ConvertOption` instances, we must define a new nominal `data` type
which we will use to index all the options of our function.

```purescript
data Flub = Flub
```

It can just be a unit type, but it may be useful to add parameters for more
dynamic configuration of conversions or to handle polymorphism.

Lets overload `bar`. We want it to take either an `Int` or a `String`.

```purescript
instance convertFlubBar1 :: ConvertOption Flub "bar" Int String where
  convertOption _ _ int = show int

instance convertFlubBar2 :: ConvertOptions Flub "bar" String String where
  convertOption _ _ str = str
```

The first two arguments can generally be ignored. They are the `Flub`
constructor and `Proxy "bar"` respectively. These are used to dispatch the
instance.

An `Int` can be converted to a `String` via `Show`, and `String` can be given
an identity conversion.

Let's overload `baz`. We want to treat it more like a nullable field. This can
be accomplished with a conversion that lifts a value with `Just`.

```purescript
instance convertFlubBaz1 :: ConvertOption Flub "baz" Boolean (Maybe Boolean) where
  convertOption _ _ bool = Just bool

instance convertFlubBaz2 :: ConvertOption Flub "baz" (Maybe Boolean) (Maybe Boolean) where
  convertOption _ _ mb = mb
```

Just like `bar`, we've provided an identity conversion.

To extend our defaulting behavior with conversions, we should use
`ConvertOptionsWithDefaults`.

```purescript
flub
  :: forall provided
   . ConvertOptionsWithDefaults Flub { | Optional } { | provided } { | All }
  => { | provided }
  -> String
flub provided = ...
  where
  all :: { | All }
  all = convertOptionsWithDefaults Flub defaultOptions provided
```

And now we have our highly-overloaded API.

> What happens if I don't write an identity conversion?

An identity conversion isn't strictly necessary, it just means you won't be able
to call the API with the "unconverted" type. This means for something like `baz`,
you could only express the absence of that option by omitting the field
altogether. This is rarely a good idea, since it means a user can't easily guard
the value on a condition.

```purescript
example = flub
  { bar: "Hello"
  , baz: guard shouldBaz *> Just true
  }
```

Instead they must write:

```purescript
example =
  if shouldBaz then
    flub { bar: "Hello", baz: true }
  else
    flub { bar: "Hello" }
```

Because it is not possible to express the absence of `baz` via `Nothing`.

> Do I need to write an identity conversion for every option, or can there be a default?

You can express your conversions as an instance chain, with a default identity
case at the end.

```purescript
instance convertFlubBar :: ConvertOption Flub "bar" Int String where
  convertOption _ _ int = show int
else instance convertFlubBaz :: ConvertOption Flub "baz" Boolean (Maybe Boolean) where
  convertOption _ _ bool = Just bool
else instance convertFlubDefault :: ConvertOption Flub option a a where
  convertOption _ _ = identity
```

In some cases, this can actually improve type inference. For example:

```purescript
example = flub { bar: "Hello", baz: Nothing }
```

Without the instance chain, this will result in an error since there is no
type annotation on `Nothing`. The compiler does not know that we want
`Maybe Boolean` rather than some other type. If we provide the identity
instance chain, then we get type-defaulting behavior, and this will typecheck
as `Maybe Boolean`.

However, one disadvantage of this approach is that users cannot extend your API
with their _own_ conversions. By avoiding instance chains, your set of options
are _extensible_ via normal typeclass machinery. That is, an end-user can
overload your API with their own types after-the-fact to suit their convenience.

```purescript
data Wat = Wat String

instance convertFlubWat :: ConvertOption Flub "bar" Wat String where
  convertOption _ _ (Wat str) = str
```

Now they can call your API with their new conversion.

## Polymorphic Options

If we wanted to extend the above API with a polymorphic option, we will need
to make a couple of adjustments.

```purescript
type Optional =
  ( foo :: Int
  , baz :: Maybe Boolean
  )

type All f =
  ( bar :: String
  , poly :: f String
  | Optional
  )

-- The polymorphic type must be added to our data type.
data Flub (f :: Type -> Type) = Flub

flub
  :: forall f provided
   . ConvertOptionsWithDefaults (Flub f) { | Optional } { | provided } { | All f }
  => Functor f
  => { | provided }
  -> String
flub provided = ...
  where
  all :: { | All f }
  all = convertOptionsWithDefaults
    (Flub :: Flub f) -- Our data type will need an annotation when called.
    defaultOptions
    provided

instance convertFlubBar :: ConvertOption (Flub f) "bar" Int String where
  convertOption _ _ int = show int
else instance convertFlubBaz :: ConvertOption (Flub f) "baz" Boolean (Maybe Boolean) where
  convertOption _ _ bool = Just bool
else instance convertFlubDefault :: ConvertOption (Flub f) option a a where
  convertOption _ _ = identity
```
