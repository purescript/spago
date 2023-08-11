# purescript-node-human-signals

A PureScript port of the [`human-signals`](https://github.com/ehmicky/human-signals) ([NPM Library](https://www.npmjs.com/package/human-signals)) JavaScript Library with type-safety enhancements.

Human-friendly process signals.

This is a map of known process signals with some information about each signal.

Unlike
[`os.constants.signals`](https://nodejs.org/api/os.html#os_signal_constants)
this includes:

- human-friendly [descriptions](#description)
- [default actions](#action), including whether they [can be prevented](#forced)
- whether the signal is [supported](#supported) by the current OS

Unlike the JavaScript library (i.e. fulfilling the library's license requirements by stating what was changed), this port:
- is written in PureScript
- provides three ways to look up a signal whereas the JS library only provides two:
    - `byNumber` - same as the JS library: lookup a signal by its number
    - `byString` - same as the JS library: lookup a signal via a dynamic string
    - `byName` - different from the JS library: lookup a signal via a static string, guaranteeing the value actually exists
- fully enumerates the possible values of the `SIGRT1`-`SIGRT31` so that they can be referenced via the `byName` field.

# Example

```purs
module Foo where

import Node.Library.HumanSignals (signals)

main :: Effect Unit
main = do
  log $ show signals.byName."SIGINT"

-- {
-- , name: "SIGINT"
-- , number: 2
-- , description: "User interruption with CTRL-C"
-- , supported: true
-- , action: Terminate
-- , forced: false
-- , standard: Ansi
-- }

  log $ show $ Map.lookup 8 signals.byNumber
-- {
-- , name: "SIGFPE"
-- , number: 8
-- , description: "Floating point arithmetic error"
-- , supported: true
-- , action: Core
-- , forced: false,
-- , standard: Ansi
-- }
```

# Install

```bash
spago install node-human-signals
```

# Usage

## name

Standard name of the signal, for example `'SIGINT'`.

## number

Code number of the signal, for example `2`. While most `number` are
cross-platform, some are different between different OS.

## description

Human-friendly description for the signal, for example
`'User interruption with CTRL-C'`.

## supported

Whether the current OS can handle this signal in Node.js using
[`process.on(name, handler)`](https://nodejs.org/api/process.html#process_signal_events).

The list of supported signals
[is OS-specific](https://github.com/ehmicky/cross-platform-node-guide/blob/main/docs/6_networking_ipc/signals.md#cross-platform-signals).

## action

What is the default action for this signal when it is not handled.

## forced

Whether the signal's default action cannot be prevented. This is `true` for
`SIGTERM`, `SIGKILL` and `SIGSTOP`.

## standard

Which standard defined that signal.
