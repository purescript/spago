-- | A “parser combinator” is a function which takes some
-- | parsers as arguments and returns a new parser.
-- |
-- | ## Combinators in other packages
-- |
-- | Many variations of well-known monadic and applicative combinators used for parsing are
-- | defined in other PureScript packages. We list some of them here.
-- |
-- | If you use a combinator from some other package for parsing, keep in mind
-- | this surprising truth about the __parsing__ package:
-- | All other combinators used with this package will be stack-safe,
-- | but usually the combinators with a `MonadRec` constraint will run faster.
-- | So you should prefer `MonadRec` versions of combinators, but for reasons
-- | of speed, not stack-safety.
-- |
-- | ### Data.Array
-- |
-- | The `many` and `many1` combinators in the __Parsing.Combinators.Array__
-- | module are faster.
-- |
-- | * [Data.Array.many](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:many)
-- | * [Data.Array.some](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:some)
-- | * [Data.Array.NonEmpty.some](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array.NonEmpty#v:some)
-- |
-- | ### Data.List
-- |
-- | The `many` and `many1` combinators in this package
-- | are redeclarations of
-- | the `manyRec` and `someRec` combinators in __Data.List__.
-- |
-- | ### Data.List.Lazy
-- |
-- | * [Data.List.Lazy.many](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:many)
-- | * [Data.List.Lazy.some](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:some)
-- |
-- | ## Combinators in this package
-- |
-- | the __replicateA__ and __replicateM__ combinators are re-exported from
-- | this module. `replicateA n p` or `replicateM n p`
-- | will repeat parser `p` exactly `n` times. The `replicateA` combinator can
-- | produce either an `Array` or a `List`.
module Parsing.Combinators
  ( try
  , tryRethrow
  , lookAhead
  , choice
  , between
  , notFollowedBy
  , option
  , optionMaybe
  , optional
  , many
  , many1
  , manyTill
  , manyTill_
  , many1Till
  , many1Till_
  , manyIndex
  , skipMany
  , skipMany1
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , endBy
  , endBy1
  , chainl
  , chainl1
  , chainr
  , chainr1
  , advance
  , withErrorMessage
  , (<?>)
  , withLazyErrorMessage
  , (<~?>)
  , asErrorMessage
  , (<??>)
  , module Control.Plus
  , module Data.Unfoldable
  , module Data.Unfoldable1
  , module Data.List.Lazy
  ) where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Plus (empty, (<|>), alt)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2, runFn5)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.List.Lazy (replicateM)
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Data.Unfoldable1 (replicate1A)
import Parsing (ParseError(..), ParseState(..), ParserT(..), Position(..), fail, parseErrorMessage, parseErrorPosition, position)

-- | Provide an error message in the case of failure.
withErrorMessage :: forall m s a. ParserT s m a -> String -> ParserT s m a
withErrorMessage p msg = p <|> fail ("Expected " <> msg)

infixl 4 withErrorMessage as <?>

-- | Provide an error message in the case of failure, but lazily. This is handy
-- | in cases where constructing the error message is expensive, so it's
-- | preferable to defer it until an error actually happens.
-- |
-- |```purescript
-- |parseBang :: Parser Char
-- |parseBang = char '!' <~?> \_ -> "a bang"
-- |```
withLazyErrorMessage :: forall m s a. ParserT s m a -> (Unit -> String) -> ParserT s m a
withLazyErrorMessage p msg = p <|> defer \_ -> fail ("Expected " <> msg unit)

infixl 4 withLazyErrorMessage as <~?>

-- | Flipped `(<?>)`.
asErrorMessage :: forall m s a. String -> ParserT s m a -> ParserT s m a
asErrorMessage = flip (<?>)

infixr 3 asErrorMessage as <??>

-- | Wrap a parser with opening and closing markers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | parens = between (string "(") (string ")")
-- | ```
between :: forall m s a open close. ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = open *> p <* close

-- | Provide a default result in the case where a parser fails without consuming input.
option :: forall m s a. a -> ParserT s m a -> ParserT s m a
option a p = p <|> pure a

-- | Optionally parse something, failing quietly.
-- |
-- | To optionally parse `p` and never fail: `optional (try p)`.
optional :: forall m s a. ParserT s m a -> ParserT s m Unit
optional p = void p <|> pure unit

-- | pure `Nothing` in the case where a parser fails without consuming input.
optionMaybe :: forall m s a. ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | If the parser fails then backtrack the input stream to the unconsumed state.
-- |
-- | One use for this combinator is to ensure that the right parser of an
-- | alternative will always be tried when the left parser fails.
-- | ```
-- | >>> runParser "ac" ((char 'a' *> char 'b') <|> (char 'a' *> char 'c'))
-- | Left (ParseError "Expected 'b'" (Position { line: 1, column: 2 }))
-- | ```
-- | ---
-- | ```
-- | >>> runParser "ac" (try (char 'a' *> char 'b') <|> (char 'a' *> char 'c'))
-- | Right 'c'
-- | ```
try :: forall m s a. ParserT s m a -> ParserT s m a
try (ParserT k1) = ParserT
  ( mkFn5 \state1@(ParseState _ _ consumed) more lift throw done ->
      runFn5 k1 state1 more lift
        ( mkFn2 \(ParseState input position _) err ->
            runFn2 throw (ParseState input position consumed) err
        )
        done
  )

-- | If the parser fails then backtrack the input stream to the unconsumed state.
-- |
-- | Like `try`, but will reposition the error to the `try` point.
-- |
-- | ```
-- | >>> runParser "ac" (try (char 'a' *> char 'b'))
-- | Left (ParseError "Expected 'b'" (Position { index: 1, line: 1, column: 2 }))
-- | ```
-- | ---
-- | ```
-- | >>> runParser "ac" (tryRethrow (char 'a' *> char 'b'))
-- | Left (ParseError "Expected 'b'" (Position { index: 0, line: 1, column: 1 }))
-- | ```
tryRethrow :: forall m s a. ParserT s m a -> ParserT s m a
tryRethrow (ParserT k1) = ParserT
  ( mkFn5 \state1@(ParseState _ position consumed) more lift throw done ->
      runFn5 k1 state1 more lift
        ( mkFn2 \(ParseState input' position' _) (ParseError err _) ->
            runFn2 throw (ParseState input' position' consumed) (ParseError err position)
        )
        done
  )

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead :: forall s a m. ParserT s m a -> ParserT s m a
lookAhead (ParserT k1) = ParserT
  ( mkFn5 \state1 more lift throw done ->
      runFn5 k1 state1 more lift
        (mkFn2 \_ err -> runFn2 throw state1 err)
        (mkFn2 \_ res -> runFn2 done state1 res)
  )

-- | Match the phrase `p` as many times as possible.
-- |
-- | If `p` never consumes input when it
-- | fails then `many p` will always succeed,
-- | but may return an empty list.
many :: forall s m a. ParserT s m a -> ParserT s m (List a)
many = List.manyRec

-- | Match the phrase `p` as many times as possible, at least once.
many1 :: forall m s a. ParserT s m a -> ParserT s m (NonEmptyList a)
many1 p = NEL.cons' <$> p <*> List.manyRec p

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepBy p sep = map NEL.toList (sepBy1 p sep) <|> pure Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1 p sep = do
  a <- p
  as <- List.manyRec $ sep *> p
  pure (NEL.cons' a as)

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy p sep = map NEL.toList (sepEndBy1 p sep) <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepEndBy1 p sep = do
  a <- p
  (NEL.cons' a <$> tailRecM go Nil) <|> pure (NEL.singleton a)
  where
  go :: List a -> ParserT s m (Step (List a) (List a))
  go acc = nextOne <|> done
    where
    nextOne = do
      -- First make sure there's a separator.
      _ <- sep
      -- Then try the phrase and loop if it's there, or bail if it's not there.
      (p <#> \a -> Loop $ a : acc) <|> done

    done = defer \_ -> pure $ Done $ reverse acc

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
endBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
endBy1 p sep = many1 $ p <* sep

-- | Parse phrases delimited and terminated by a separator.
endBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endBy p sep = List.manyRec $ p <* sep

-- | `chainl p f` parses one or more occurrences of `p`, separated by operator `f`.
-- |
-- | Returns a value
-- | obtained by a left-associative application of the functions returned by
-- | `f` to the values returned by `p`. This combinator can be used to
-- | eliminate left-recursion in expression grammars.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainl digit (string "+" $> add) 0
-- | ```
chainl :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl p f a = chainl1 p f <|> pure a

-- | `chainl` requiring at least one match.
chainl1 :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1 p f = do
  a <- p
  tailRecM go a
  where
  go :: a -> ParserT s m (Step a a)
  go a =
    ( do
        op <- f
        a' <- p
        pure $ Loop $ op a a'
    )
      <|> pure (Done a)

-- | `chainr p f` parses one or more occurrences of `p`, separated by operator `f`.
-- |
-- | Returns a value
-- | obtained by a right-associative application of the functions returned by
-- | `f` to the values returned by `p`. This combinator can be used to
-- | eliminate right-recursion in expression grammars.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" $> add) 0
-- | ```
chainr :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr p f a = chainr1 p f <|> pure a

-- | `chainr` requiring at least one match.
chainr1 :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1 p f = do
  a <- p
  tailRecM go { last: a, init: Nil }
  where
  -- This looks scary at first glance, so I'm leaving a comment in a vain
  -- attempt to explain how it works.
  --
  -- The loop state is a record {init, last}, where `last` is the last (i.e.
  -- rightmost) `a` value that has been parsed so far, and `init` is a list of
  -- (value + operator) pairs that have been parsed before that.
  --
  -- The very first value is parsed at top level, and it becomes the initial
  -- value of `last`, while the initial value of `init` is just `Nil`,
  -- indicating that no pairs of (value + operator) have been parsed yet.
  --
  -- At every step, we parse an operator and a value, and then the newly parsed
  -- value becomes `last` (because, well, it's been parsed last), and the pair
  -- of (previous last + operator) is prepended to `init`.
  --
  -- After we can no longer parse a pair of (value + operation), we're done. At
  -- that point, we have a list of (value + operation) pairs in reverse order
  -- (since we prepend each pair as we go) and the very last value. All that's
  -- left is combine them all via `foldl`.
  go
    :: { init :: List (a /\ (a -> a -> a)), last :: a }
    -> ParserT s m
         ( Step
             { init :: List (a /\ (a -> a -> a)), last :: a }
             a
         )
  go { last, init } =
    ( do
        op <- f
        a <- p
        pure $ Loop { last: a, init: (last /\ op) : init }
    )
      <|> defer \_ -> pure (Done $ foldl apply last init)

  apply :: a -> (a /\ (a -> a -> a)) -> a
  apply y (x /\ op) = x `op` y

-- | Parse one of a set of alternatives.
choice :: forall f m s a. Foldable f => f (ParserT s m a) -> ParserT s m a
choice = fromMaybe empty <<< foldr go Nothing
  where
  go p1 = case _ of
    Nothing -> Just p1
    Just p2 -> Just (p1 <|> p2)

-- | Skip many instances of a phrase.
skipMany :: forall s a m. ParserT s m a -> ParserT s m Unit
skipMany p = skipMany1 p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall s a m. ParserT s m a -> ParserT s m Unit
skipMany1 p = p *> tailRecM go unit
  where
  go _ = (p $> Loop unit) <|> pure (Done unit)

-- | Fail if the parser succeeds.
-- |
-- | Will never consume input.
notFollowedBy :: forall s a m. ParserT s m a -> ParserT s m Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Parse many phrases until the terminator phrase matches.
manyTill :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (List a)
manyTill p end = tailRecM go Nil
  where
  go :: List a -> ParserT s m (Step (List a) (List a))
  go acc =
    (end <#> \_ -> Done $ reverse acc)
      <|> (p <#> \x -> Loop $ x : acc)

-- | Parse at least one phrase until the terminator phrase matches.
many1Till :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (NonEmptyList a)
many1Till p end = NEL.cons' <$> p <*> manyTill p end

-- | Parse many phrases until the terminator phrase matches, requiring at least one match.
-- | Returns the list of phrases and the terminator phrase.
many1Till_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (NonEmptyList a) e)
many1Till_ p end = do
  x <- p
  Tuple xs t <- manyTill_ p end
  pure $ Tuple (cons' x xs) t

-- | Parse many phrases until the terminator phrase matches.
-- | Returns the list of phrases and the terminator phrase.
-- |
-- | #### Non-greedy repetition
-- |
-- | Use the __manyTill_ __ combinator
-- | to do non-greedy repetition of a pattern `p`, like we would in Regex
-- | by writing `p*?`.
-- | To repeat pattern `p` non-greedily, write
-- | `manyTill_ p q` where `q` is the entire rest of the parser.
-- |
-- | For example, this parse fails because `many` repeats the pattern `letter`
-- | greedily.
-- |
-- | ```
-- | runParser "aab" do
-- |   a <- many letter
-- |   b <- char 'b'
-- |   pure (Tuple a b)
-- | ```
-- | ```
-- | (ParseError "Expected 'b'" (Position { line: 1, column: 4 }))
-- | ```
-- |
-- | To repeat pattern `letter` non-greedily, use `manyTill_`.
-- |
-- | ```
-- | runParser "aab" do
-- |   Tuple a b <- manyTill_ letter do
-- |     char 'b'
-- |   pure (Tuple a b)
-- | ```
-- | ```
-- | (Tuple ('a' : 'a' : Nil) 'b')
-- | ```
manyTill_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (List a) e)
manyTill_ p end = tailRecM go Nil
  where
  go :: List a -> ParserT s m (Step (List a) (Tuple (List a) e))
  go xs = alt
    do
      t <- end
      pure (Done (Tuple (reverse xs) t))
    do
      x <- p
      pure (Loop (x : xs))

-- | Parse the phrase as many times as possible, at least *N* times, but no
-- | more than *M* times.
-- | If the phrase can’t parse as least *N* times then the whole
-- | parser fails. If the phrase parses successfully *M* times then stop.
-- | The current phrase index, starting at *0*, is passed to the phrase.
-- |
-- | Returns the list of parse results and the number of results.
-- |
-- | `manyIndex n n (\_ -> p)` is equivalent to `replicateA n p`.
manyIndex :: forall s m a. Int -> Int -> (Int -> ParserT s m a) -> ParserT s m (Tuple Int (List a))
manyIndex from to p =
  if from > to || from < 0 then
    pure (Tuple 0 Nil)
  else
    tailRecM go (Tuple 0 Nil)
  where
  go (Tuple i xs) =
    if i >= to then
      pure (Done (Tuple i (reverse xs)))
    else catchError
      do
        x <- p i
        pure (Loop (Tuple (i + 1) (x : xs)))
      \e -> do
        if i >= from then
          pure (Done (Tuple i (reverse xs)))
        else
          throwError $ ParseError
            (parseErrorMessage e <> " (at least " <> show from <> ", but only parsed " <> show i <> ")")
            (parseErrorPosition e)

-- | If the parser succeeds without advancing the input stream position,
-- | then force the parser to fail.
-- |
-- | This combinator can be used to prevent infinite parser repetition.
-- |
-- | Does not depend on or effect the `consumed` flag which indicates whether
-- | we are committed to this parsing branch.
advance :: forall s m a. ParserT s m a -> ParserT s m a
advance p = do
  Position { index: index1 } <- position
  x <- p
  Position { index: index2 } <- position
  if index2 > index1 then
    pure x
  else
    fail "Expected progress"
