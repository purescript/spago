# Parsing

[![CI](https://github.com/purescript-contrib/purescript-parsing/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-parsing/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-parsing.svg)](https://github.com/purescript-contrib/purescript-parsing/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-parsing/badge)](https://pursuit.purescript.org/packages/purescript-parsing)
[![Maintainer: jamesdbrock](https://img.shields.io/badge/maintainer-jamesdbrock-teal.svg)](https://github.com/jamesdbrock)
[![Maintainer: robertdp](https://img.shields.io/badge/maintainer-robertdp-teal.svg)](https://github.com/robertdp)
[![Maintainer: chtenb](https://img.shields.io/badge/maintainer-chtenb-teal.svg)](http://github.com/chtenb)

A monadic parser combinator library based on Haskell’s
[Parsec](https://hackage.haskell.org/package/parsec).

## Installation

Install `parsing` with [Spago](https://github.com/purescript/spago):

```console
$ spago install parsing
```

## Quick start

Here is a basic tutorial introduction to monadic parsing with this package.

### Parsers

A parser turns a string into a data structure. Parsers in this library have the type `Parser s a`, where `s` is the type of the input string, and `a` is the type of the data which the parser will produce on success. `Parser s` is a monad. It’s defined in the module `Parsing`.

Monads can be used to provide context for a computation, and that’s how we use them in monadic parsing.
The context provided by the `Parser s` monad is __the parser’s current location in the input string__.
Parsing starts at the beginning of the input string.

Parsing requires two more capabilities: __alternative__ and __failure__.

We need __alternative__ to be able to choose what kind of thing we’re parsing depending
on the input which we encounter. This is provided by the  `<|>` “alt”
operator of the `Alt` typeclass instance of the `Parser s` monad.
The expression `p_left <|> p_right` will first try the `p_left` parser and if that fails
__and consumes no input__ then it will try the `p_right` parser.

We need __failure__ in case the input stream is not parseable. This is provided by the `fail`
function, which calls the `throwError` function of the `MonadThrow` typeclass instance of
the `Parser s` monad.

To run a parser, call the function `runParser :: s -> Parser s a -> Either ParseError a` in
the `Parsing` module, and supply it with an input string and a parser.
If the parse succeeds then the result is `Right a` and if the parse fails then the
result is `Left ParseError`.

### Primitive parsers

Each type of input string needs primitive parsers.
Primitive parsers for input string type `String` are in the `Parsing.String` module.
For example, the primitive `char :: Char -> Parser String Char` parser will exactly match
one literal character and then advance by one position in the input string.

We can use these primitive parsers to write other `String` parsers.

### Writing a parser

Here is a parser `ayebee :: Parser String Boolean` which will accept only two input
strings: `"ab"` or `"aB"`.
It will return `true` if the `b` character is uppercase.
It will return `false` if the `b` character is lowercase.
It will fail with a `ParseError` if the input string is anything else.

```purescript
ayebee :: Parser String Boolean
ayebee = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure (b == 'B')
```

We can run the parser `ayebee` like so

```purescript
runParser "aB" ayebee
```

and then the parser will succeed and return `Right true`.

#### [✨ Run the `ayebee` parser in your browser on *Try PureScript!*](https://try.purescript.org/?github=/purescript-contrib/purescript-parsing/main/docs/examples/QuickStart.purs)

### More parsers

There are other `String` parsers in the module `Parsing.String.Basic`, for example the parser `letter :: Parser String Char` which will accept any single alphabetic letter.

### Parser combinators

Parser combinators are in this package in the module `Parsing.Combinators`.

A parser combinator is a function which takes a parser as an argument and returns a new parser. The `many` combinator, for example, will repeat a parser as many times as it can. So the parser `many letter` will have type `Parser String (Array Char)`.

Running the parser

```purescript
runParser "aBabaB" (many ayebee)
```

will return `Right [true, false, true]`.

## Stack-safety

Starting with v9.0.0, all parsers and combinators in this package are always
stack-safe.

## Recursion

For the most part, we can just write recursive parsers (parsers defined in
terms of themselves) and they will work as we expect.

In some cases like this:

```purescript
aye :: Parser String Char
aye = char 'a' *> aye
```

we might get a compile-time *CycleInDeclaration* error which looks like this:

```
  The value of aye is undefined here, so this reference is not allowed.


See https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md for more information,
or to contribute content related to this error.
```

This is happening because we tried to call `aye` recursively __“at a point
where such a reference would be unavailable because of *strict evaluation*.”__

The
[best way to solve](https://discourse.purescript.org/t/parsing-recursively-with-purescript-parsing/3184/2)
this is to stick a
[`Control.Lazy.defer`](https://pursuit.purescript.org/packages/purescript-control/docs/Control.Lazy#t:Lazy)
in front of the parser to break the cycle.
```purescript
aye :: Parser String Char
aye = defer \_ -> char 'a' *> aye
```



## Resources

- [*Monadic Parsers at the Input Boundary* (YouTube)](https://www.youtube.com/watch?v=LLkbzt4ms6M) by James Brock is an introductory tutorial to monadic parser combinators with this package.

- [*Monadic Parser Combinators*](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) by Graham Hutton and Erik Meijer 1996.

- The original short classic [FUNCTIONAL PEARLS *Monadic Parsing in Haskell*](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) by Graham Hutton and Erik Meijer 1998.

- [*Parsec: Direct Style Monadic Parser Combinators For The Real World*](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf) by Daan Leijen and Erik Meijer 2001.

- [*Parse, don't validate*](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) by Alexis King is about what it means to “parse” something, without any mention of monads.

- [*Revisiting Monadic Parsing in Haskell*](https://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/) by Vaibhav Sagar is a reflection on the Hutton, Meijer FUNCTIONAL PEARL.

- [*Parsec: “try a <|> b” considered harmful*](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/) by Edward Z. Yang is about how to decide when to backtrack
from a failed alternative.

- [*Parser Combinators in Haskell*](https://serokell.io/blog/parser-combinators-in-haskell) by Heitor Toledo Lassarote de Paula.

There are lots of other great monadic parsing tutorials on the internet.

## Related Packages

- [__`parsing-dataview`__](https://pursuit.purescript.org/packages/purescript-parsing-dataview) primitive parsers for binary parsing of `ArrayBuffer`.
- [__`datetime-parsing`__](https://pursuit.purescript.org/packages/purescript-datetime-parsing) for parsing dates and times.

## Documentation

`parsing` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-parsing).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-parsing/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `parsing` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-parsing/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
