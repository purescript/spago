-- | Primitive parsers, combinators and functions for working with an input
-- | stream of type `String`.
-- |
-- | All of these primitive parsers will consume when they succeed.
-- |
-- | All of these primitive parsers will not consume and will automatically
-- | backtrack when they fail.
-- |
-- | The behavior of these primitive parsers is based on the behavior of the
-- | `Data.String` module in the __strings__ package.
-- | In most JavaScript runtime environments, the `String`
-- | is little-endian [UTF-16](https://en.wikipedia.org/wiki/UTF-16).
-- |
-- | The primitive parsers which return `Char` will only succeed when the character
-- | being parsed is a code point in the
-- | [Basic Multilingual Plane](https://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane)
-- | (the “BMP”). These parsers can be convenient because of the good support
-- | that PureScript has for writing `Char` literals like `'あ'`, `'β'`, `'C'`.
-- |
-- | The other primitive parsers, which return `CodePoint` and `String` types,
-- | can parse the full Unicode character set. All of the primitive parsers
-- | in this module can be used together.
-- |
-- | ### Position
-- |
-- | In a `String` parser, the `Position {index}` counts the number of
-- | unicode `CodePoint`s since the beginning of the input string.
-- |
-- | Each tab character (`0x09`) encountered in a `String` parser will advance
-- | the `Position {column}` by 8.
-- |
-- | These patterns will advance the `Position {line}` by 1 and reset
-- | the `Position {column}` to 1:
-- | - newline (`0x0A`)
-- | - carriage-return (`0x0D`)
-- | - carriage-return-newline (`0x0D 0x0A`)
module Parsing.String
  ( char
  , string
  , anyChar
  , anyCodePoint
  , satisfy
  , satisfyCodePoint
  , takeN
  , rest
  , eof
  , match
  , regex
  , anyTill
  , consumeWith
  , parseErrorHuman
  ) where

import Prelude hiding (between)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (replicate)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Function.Uncurried (mkFn5, runFn2)
import Data.Int (odd)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (CodePoint, Pattern(..), codePointAt, length, null, splitAt, stripPrefix, uncons)
import Data.String as CodePoint
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags)
import Data.Tuple (Tuple(..))
import Parsing (ParseError(..), ParseState(..), ParserT(..), Position(..), getParserT)
import Parsing.Combinators (alt, try, (<?>))
import Partial.Unsafe (unsafePartial)

-- | Match “end-of-file,” the end of the input stream.
eof :: forall m. ParserT String m Unit
eof = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      if null input then
        -- We must consume so this combines correctly with notFollowedBy
        runFn2 done (ParseState input pos true) unit
      else
        runFn2 throw state1 (ParseError "Expected EOF" pos)
  )

-- | Match the entire rest of the input stream. Always succeeds.
rest :: forall m. ParserT String m String
rest = consumeWith \consumed ->
  Right { value: consumed, consumed, remainder: "" }

-- | Match the specified string.
string :: forall m. String -> ParserT String m String
string str = consumeWith \input ->
  case stripPrefix (Pattern str) input of
    Just remainder ->
      Right { value: str, consumed: str, remainder }
    _ ->
      Left $ "Expected " <> show str

-- | Match any BMP `Char`.
-- | Parser will fail if the character is not in the Basic Multilingual Plane.
anyChar :: forall m. ParserT String m Char
anyChar = satisfy (const true)

-- | Match any Unicode character.
-- | Always succeeds when any input remains.
anyCodePoint :: forall m. ParserT String m CodePoint
anyCodePoint = satisfyCodePoint (const true)

-- | Match a BMP `Char` satisfying the predicate.
satisfy :: forall m. (Char -> Boolean) -> ParserT String m Char
satisfy f = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      case uncons input of
        Nothing ->
          runFn2 throw state1 (ParseError "Unexpected EOF" pos)
        Just { head, tail } -> do
          let cp = fromEnum head
          -- the `fromEnum` function doesn't check if this is beyond the
          -- BMP, so we check that ourselves.
          -- https://github.com/purescript/purescript-strings/issues/153
          if cp < 0 || cp > 65535 then
            runFn2 throw state1 (ParseError "Expected Char" pos)
          else do
            let ch = unsafePartial (fromJust (toEnum cp))
            if f ch then
              runFn2 done (ParseState tail (updatePosSingle pos head tail) true) ch
            else
              runFn2 throw state1 (ParseError "Predicate unsatisfied" pos)
  )

-- | Match a Unicode character satisfying the predicate.
satisfyCodePoint :: forall m. (CodePoint -> Boolean) -> ParserT String m CodePoint
satisfyCodePoint f = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      case uncons input of
        Nothing ->
          runFn2 throw state1 (ParseError "Unexpected EOF" pos)
        Just { head, tail } ->
          if f head then
            runFn2 done (ParseState tail (updatePosSingle pos head tail) true) head
          else
            runFn2 throw state1 (ParseError "Predicate unsatisfied" pos)
  )

-- | Match the specified BMP `Char`.
char :: forall m. Char -> ParserT String m Char
char c = satisfy (_ == c) <?> show c

-- | Match a `String` exactly *N* characters long.
takeN :: forall m. Int -> ParserT String m String
takeN n = consumeWith \input -> do
  let { before, after } = splitAt n input
  if length before == n then
    Right { value: before, consumed: before, remainder: after }
  else
    Left $ "Could not take " <> show n <> " characters"

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> String -> Position
updatePosString pos before after = case uncons before of
  Nothing -> pos
  Just { head, tail } -> do
    let
      newPos
        | String.null tail = updatePosSingle pos head after
        | otherwise = updatePosSingle pos head tail
    updatePosString newPos tail after

-- | Updates a `Position` by adding the columns and lines in a
-- | single `CodePoint`.
updatePosSingle :: Position -> CodePoint -> String -> Position
updatePosSingle (Position { index, line, column }) cp after = case fromEnum cp of
  10 -> Position { index: index + 1, line: line + 1, column: 1 } -- "\n"
  13 ->
    case codePointAt 0 after of
      Just nextCp | fromEnum nextCp == 10 -> Position { index: index + 1, line, column } -- "\r\n" lookahead
      _ -> Position { index: index + 1, line: line + 1, column: 1 } -- "\r"
  9 -> Position { index: index + 1, line, column: column + 8 - ((column - 1) `mod` 8) } -- "\t" Who says that one tab is 8 columns?
  _ -> Position { index: index + 1, line, column: column + 1 }

-- | Combinator which returns both the result of a parse and the slice of
-- | the input that was consumed while it was being parsed.
match :: forall m a. ParserT String m a -> ParserT String m (Tuple String a)
match p = do
  ParseState input1 _ _ <- getParserT
  x <- p
  ParseState input2 _ _ <- getParserT
  -- We use the `SCU.length`, which is in units of “code units”
  -- instead of `Data.String.length`. which is in units of “code points”.
  -- This is more efficient, and it will be correct as long as we can assume
  -- the invariant that the `ParseState input` always begins on a code point
  -- boundary.
  pure $ Tuple (SCU.take (SCU.length input1 - SCU.length input2) input1) x

-- | Compile a regular expression `String` into a regular expression parser.
-- |
-- | This function will use the `Data.String.Regex.regex` function to compile
-- | and return a parser which can be used
-- | in a `ParserT String m` monad.
-- | If compilation fails then this function will return `Left` a compilation
-- | error message.
-- |
-- | The returned parser will try to match the regular expression pattern once,
-- | starting at the current parser position. On success, it will return
-- | the matched substring.
-- |
-- | If the RegExp `String` is constant then we can assume that compilation will
-- | always succeed and `unsafeCrashWith` if it doesn’t. If we dynamically
-- | generate the RegExp `String` at runtime then we should handle the
-- | case where compilation of the RegExp fails.
-- |
-- | This function should be called outside the context of a `ParserT String m`
-- | monad for two reasons:
-- | 1. If we call this function inside of the `ParserT String m` monad and
-- |    then `fail` the parse when the compilation fails,
-- |    then that could be confusing because a parser failure is supposed to
-- |    indicate an invalid input string.
-- |    If the compilation failure occurs in an `alt` then the compilation
-- |    failure might not be reported at all and instead
-- |    the input string would be parsed incorrectly.
-- | 2. Compiling a RegExp is expensive and it’s better to do it
-- |    once in advance and then use the compiled RegExp many times than
-- |    to compile the RegExp many times during the parse.
-- |
-- | This parser may be useful for quickly consuming a large section of the
-- | input `String`, because in a JavaScript runtime environment a compiled
-- | RegExp is a lot faster than a monadic parser built from parsing primitives.
-- |
-- | [*MDN Regular Expressions Cheatsheet*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet)
-- |
-- | #### Example
-- |
-- | This example shows how to compile and run the `xMany` parser which will
-- | capture the regular expression pattern `x*`.
-- |
-- | ```purescript
-- | case regex "x*" noFlags of
-- |   Left compileError -> unsafeCrashWith $ "xMany failed to compile: " <> compileError
-- |   Right xMany -> runParser "xxxZ" do
-- |     xMany
-- | ```
-- |
-- | #### Flags
-- |
-- | Set `RegexFlags` with the `Semigroup` instance like this.
-- |
-- | ```purescript
-- | regex "x*" (dotAll <> ignoreCase)
-- | ```
-- |
-- | The `dotAll`, `unicode`, and `ignoreCase` flags might make sense for
-- | a `regex` parser. The other flags will
-- | probably cause surprising behavior and you should avoid them.
-- |
-- | [*MDN Advanced searching with flags*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#advanced_searching_with_flags)
regex :: forall m. String -> RegexFlags -> Either String (ParserT String m String)
regex pattern flags =
  Regex.regex ("^(" <> pattern <> ")") flags <#> \regexobj ->
    consumeWith \input -> do
      case NonEmptyArray.head <$> Regex.match regexobj input of
        Just (Just consumed) -> do
          let remainder = SCU.drop (SCU.length consumed) input
          Right { value: consumed, consumed, remainder }
        _ ->
          Left "No Regex pattern match"

-- | Consume a portion of the input string while yielding a value.
-- |
-- | Takes a consumption function which takes the remaining input `String`
-- | as its argument and returns either an error message, or three fields:
-- |
-- | * `value` is the value to return.
-- | * `consumed` is the input `String` that was consumed. It is used to update the parser position.
-- |   If the `consumed` `String` is non-empty then the `consumed` flag will
-- |   be set to true. (Confusing terminology.)
-- | * `remainder` is the new remaining input `String`.
-- |
-- | This function is used internally to construct primitive `String` parsers.
consumeWith
  :: forall m a
   . (String -> Either String { value :: a, consumed :: String, remainder :: String })
  -> ParserT String m a
consumeWith f = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      case f input of
        Left err ->
          runFn2 throw state1 (ParseError err pos)
        Right { value, consumed, remainder } ->
          runFn2 done (ParseState remainder (updatePosString pos consumed remainder) (not (String.null consumed))) value
  )

-- | Combinator which finds the first position in the input `String` where the
-- | phrase can parse. Returns both the
-- | parsed result and the unparsable input section searched before the parse.
-- | Will fail if no section of the input is parseable. To backtrack the input
-- | stream on failure, combine with `tryRethrow`.
-- |
-- | This combinator works like
-- | [Data.String.takeWhile](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:takeWhile)
-- | or
-- | [Data.String.Regex.search](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:search)
-- | and it allows using a parser for the pattern search.
-- |
-- | This combinator is equivalent to `manyTill_ anyCodePoint`, but it will be
-- | faster because it returns a slice of the input `String` for the
-- | section preceding the parse instead of a `List CodePoint`.
-- |
-- | Be careful not to look too far
-- | ahead; if the phrase parser looks to the end of the input then `anyTill`
-- | could be *O(n²)*.
anyTill
  :: forall m a
   . Monad m
  => ParserT String m a
  -> ParserT String m (Tuple String a)
anyTill p = do
  ParseState input1 _ _ <- getParserT
  Tuple input2 t <- tailRecM go unit
  pure $ Tuple (SCU.take (SCU.length input1 - SCU.length input2) input1) t
  where
  go unit = alt
    ( do
        ParseState input2 _ _ <- getParserT
        t <- try p
        pure $ Done $ Tuple input2 t
    )
    ( do
        _ <- anyCodePoint
        pure $ Loop unit
    )

-- | Returns three `String`s which, when printed line-by-line, will show
-- | a human-readable parsing error message with context.
-- |
-- | #### Input arguments
-- |
-- | * The first argument is the input `String` given to the parser which
-- | errored.
-- | * The second argument is a positive `Int` which indicates how many
-- | characters of input `String` context are wanted around the parsing error.
-- | * The third argument is the `ParseError` for the input `String`.
-- |
-- | #### Output `String`s
-- |
-- | 1. The parse error message and the parsing position.
-- | 2. A string with an arrow that points to the error position in the
-- |    input context (in a fixed-width font).
-- | 3. The input context. A substring of the input which tries to center
-- |    the error position and have the wanted length and not include
-- |    any newlines or carriage returns.
-- |
-- |    If the parse error occurred on a carriage return or newline character,
-- |    then that character will be included at the end of the input context.
-- |
-- | #### Example
-- |
-- | ```
-- | let input = "12345six789"
-- | case runParser input (replicateA 9 String.Basic.digit) of
-- |   Left err ->
-- |     log $ String.joinWith "\n" $ parseErrorHuman input 20 err
-- | ```
-- | ---
-- | ```
-- | Expected digit at position index:5 (line:1, column:6)
-- |      ▼
-- | 12345six789
-- | ```
parseErrorHuman :: String -> Int -> ParseError -> Array String
parseErrorHuman input contextSize (ParseError msg (Position { line, column, index })) =
  -- inspired by
  -- https://github.com/elm/parser/blob/master/README.md#tracking-context
  [ msg <> " at position index:" <> show index
      <> " (line:"
      <> show line
      <> ", column:"
      <> show column
      <> ")"
  , (String.joinWith "" (replicate (lineIndex - minPosBefore) " ")) <> "▼" -- best way to construct string of spaces?
  , inputContext
  ]
  where
  -- select the input line in which the error appears
  -- sadly we can't use splitCap because of circular module dependency and we
  -- don't feel like separating out an “Internal” module.
  { posBegin, posEnd, lineBegin } = go 0 input 0 input
    where
    go posBegin lineBegin posEnd lineEnd =
      case String.uncons lineEnd of
        Just { head, tail } | head == CodePoint.codePointFromChar '\n' ->
          if posEnd == index -- uh-oh, error at the newline
          -- so include the newline at the end of the selected line.
          then { posBegin, posEnd: posEnd + 1, lineBegin }
          else if posEnd > index then { posBegin, posEnd, lineBegin }
          else go (posEnd + 1) tail (posEnd + 1) tail
        Just { head, tail } | head == CodePoint.codePointFromChar '\r' ->
          if posEnd == index -- uh-oh, error at the carriage return
          -- so include the carriage return at the end of the selected line.
          -- we don't need to add the possible following newline because
          -- we're not printing a line break here, we're just making sure
          -- to include the character at the position which errored.
          then { posBegin, posEnd: posEnd + 1, lineBegin }
          else if posEnd > index then { posBegin, posEnd, lineBegin }
          else go (posEnd + 1) tail (posEnd + 1) tail
        Just { tail } -> go posBegin lineBegin (posEnd + 1) tail
        _ -> { posBegin, posEnd, lineBegin }
  lineSelect = String.take (posEnd - posBegin) lineBegin
  lineIndex = index - posBegin
  lineLength = String.length lineSelect

  -- position minus half of context
  bestPosBefore = lineIndex - (contextSize / 2)
  -- position plus half of context
  bestPosAfter = lineIndex + (contextSize / 2) + if odd contextSize then 1 else 0

  -- constrain the context window to selected line
  -- grow the context window to contextSize if the error is at beginning or end of selected line
  Tuple minPosBefore maxPosAfter =
    if bestPosBefore >= 0 then
      if bestPosAfter <= lineLength then Tuple bestPosBefore bestPosAfter
      else Tuple (max 0 (lineLength - contextSize)) lineLength
    else Tuple 0 (min lineLength contextSize)

  inputContext = String.take (maxPosAfter - minPosBefore) $ String.drop minPosBefore lineSelect
