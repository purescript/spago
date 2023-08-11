# purescript-foreign-object

[![Latest release](http://img.shields.io/github/release/purescript/purescript-foreign-object.svg)](https://github.com/purescript/purescript-foreign-object/releases)
[![Build status](https://github.com/purescript/purescript-foreign-object/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-foreign-object/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-foreign-object/badge)](https://pursuit.purescript.org/packages/purescript-foreign-object)

Functions for working with homogeneous JavaScript objects from PureScript. Similar to using `Map String a` but simply reusing JavaScript objects.

## Installation

```
spago install foreign-object
```

## Example

```purs
example = do
  let
    -- make an empty Object
    empty = FO.empty

    -- insert to an empty Object
    inserted = FO.insert "a" 1 empty

    -- or: use the singleton function
    -- singleton FO.singleton "a" 1

  -- lookup values for existing in the Object as a result of Maybe
  let lookup = FO.lookup "a" inserted
  Assert.assertEqual { actual: lookup, expected: Just 1 }

  -- delete a value from an Object
  let deleted = FO.delete "a" inserted
  Assert.assertEqual { actual: deleted, expected: FO.empty }

  let
    -- convert homogeneous records to Object
    converted = FO.fromHomogeneous { a: 1, b: 2, c: 3}
    -- check that the converted is equal to a regularly built Object
    built
      = FO.empty
      # FO.insert "a" 1
      # FO.insert "b" 2
      # FO.insert "c" 3

  Assert.assertEqual { actual: converted, expected: built }
```

See the [tests](test/Main.purs) for more examples.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-foreign-object).
