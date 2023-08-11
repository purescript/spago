# purescript-psci-support

[![Latest release](http://img.shields.io/github/release/purescript/purescript-psci-support.svg)](https://github.com/purescript/purescript-psci-support/releases)
[![Build status](https://github.com/purescript/purescript-psci-support/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-psci-support/actions?query=workflow%3ACI+branch%3Amaster)

Support module for the PSCI interactive mode. This package is required for correct operation of PSCI as of version 0.9.0.

## Installation

```
spago install psci-support
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-psci-support).

## Notes for Library Implementors

If you are implementing an alternative to the Prelude, you may wish to implement the `Eval` class for any types which should support evaluation in the REPL.
