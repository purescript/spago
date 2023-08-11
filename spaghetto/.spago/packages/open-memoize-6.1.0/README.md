# purescript-memoize

Type classes for creating memoized functions.

## Building

This module can be added to your project using Bower.

To work on this project, use `pulp`:

```text
$ pulp build

$ pulp test
```

## Example

The following Fibonacci function implementation is slow, because its call graph grows exponentially in the size of its argument:

```purescript
let fibonacciSlow 0 = 0
    fibonacciSlow 1 = 1
    fibonacciSlow n = fibonacciSlow (n - 1) +
                      fibonacciSlow (n - 2)
```

The `memoize` function can be used to improve the performance of this function, by tabulating intermediate results:

```purescript
let fibonacciSlow 0 = 0
    fibonacciSlow 1 = 1
    fibonacciSlow n = fibonacci (n - 1) +
                      fibonacci (n - 2)

    fibonacci = memoize $ \n -> fibonacciSlow n
```

Note that `fibonacciSlow` has been modified to call the faster `fibonacci` function.

The `memoize` function can be applied whenever there is a `Tabulate` instance for the function argument type. This library provides instances of the `Tabulate` type class for common types, such as `Maybe`, `Either` and `Tuple`.

## Related resources

- Slides: [Elegant memoization](https://github.com/conal/talk-2014-elegant-memoization/blob/master/README.md) by Conal Elliott - @conal
