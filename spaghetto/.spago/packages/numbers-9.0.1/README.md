# purescript-numbers

[![Latest release](http://img.shields.io/github/release/purescript/purescript-numbers.svg)](https://github.com/purescript/purescript-numbers/releases)
[![Build status](https://github.com/purescript/purescript-numbers/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-numbers/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-numbers/badge)](https://pursuit.purescript.org/packages/purescript-numbers)

Utility functions for working with PureScripts builtin `Number` type.

## Installation

```
spago install numbers
```

## Scope

* Parsing with `fromString`
* Formating with `toStringWith`, see `Data.Number.Format`
* Approximate comparisions with `≅`, see `Data.Number.Approximate`
* Not-a-number and infinite value detection with `isNaN` and `isFinite`
* Remainder with `%`
* Trignometric functions with `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, and
  `atan2`
* Natural logarithm and exponents with `log` and `exp`
* Powers with `sqrt` and `pow`
* Rounding with `ceil`, `floor`, `round`, and `trunc`
* Numeric minimum and maximum with `min` and `max`, which behave differently to
  the versions in `Data.Ord` on values of `NaN`
* Sign and absolute value functions `sign` and `abs`
* Numeric constants `e`, `ln 2`, `ln10`, `log10e`, `log2e`, `pi`, `sqrt1_2`,
  `sqrt2`, and `tau`

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-numbers).
