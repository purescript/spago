# purescript-aff-promise

Simple library for interop between Aff and JavaScript promises.

No typeclass instances etc are provided to use Promises directly - the intention is that your PureScript code uses Aff
internally, and is only wrapped as a Promise to present an API for consumption in JavaScript code (and vice versa).

## Exposing promises: Aff to Promise

The origin of this library was to solve the following problem: I want to write asynchronous code in `Aff`, but I need
to expose this as an API to JavaScript in the form of a `Promise`.

Therefore the majority of code can be written to return an `Aff`, in this example using `readTextFile` from 
[purescript-node-fs-aff](https://pursuit.purescript.org/packages/purescript-node-fs-aff):

```purescript
getLinesAff :: Aff (Array String)
getLinesAff = do
  text <- readTextFile Encoding.UTF8 "data.txt"
  pure $ split (Pattern "\n") text
```

and this can be exposed as a function returning a promise:

```purescript
import Control.Promise as Promise

getLines :: Effect (Promise (Array String))
getLines = Promise.fromAff getLinesAff
```

This function returns an `Effect` beause a promise is "already running". A JavaScript function consuming this API
would just call `getLines().then(...)`.

## Consuming promises: Promise to Aff

The reverse problem occurs when wrapping a JS library that uses promises for asynchronous behaviour (`Aff` comes
with a perfectly good function to deal with consuming libraries that use callbacks).

Consider the case of consuming the `fetch` API.

```javascript
exports.fetchImpl = function (url) {
  return function() {
    return fetch(url);
  };
}
```

```purescript
foreign import data Response :: Type
foreign import fetchImpl :: String -> Effect (Promise Response)
```

Notice that the `fetch(url)` call is wrapped in a thunk and imported as an `Effect`, because otherwise the `fetch`
operation would be initiated as a side-effect of a pure function - while `fetch` returns a promise, at the point
the function returns the network request has already been initated.

The response can then be converted to an `Aff` and consumed easily:

```purescript
import Control.Promise as Promise

fetch :: String -> Aff String
fetch = fetchImpl >>> Promise.toAffE
```

# Documentation

API documentation is available [on Pursuit](https://pursuit.purescript.org/packages/purescript-aff-promise).
