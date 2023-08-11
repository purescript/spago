-- | Types and operations for monadic parsing.
-- |
-- | Combinators are in the `Parsing.Combinators` module.
-- |
-- | Primitive parsers for `String` input streams are in the `Parsing.String`
-- | module.
module Parsing
  ( Parser
  , runParser
  , ParserT(..)
  , runParserT
  , runParserT'
  , ParseError(..)
  , parseErrorMessage
  , parseErrorPosition
  , Position(..)
  , initialPos
  , consume
  , position
  , fail
  , failWithPosition
  , region
  , liftMaybe
  , liftEither
  , liftExceptT
  , ParseState(..)
  , stateParserT
  , getParserT
  , hoistParserT
  , mapParserT
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class Alternative, class MonadPlus, class Plus)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn5, mkFn2, mkFn3, mkFn5, runFn2, runFn3, runFn5)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst)

-- | A parsing error, consisting of an error message and
-- | the position in the input stream at which the error occurred.
data ParseError = ParseError String Position

-- | Get the `Message` from a `ParseError`
parseErrorMessage :: ParseError -> String
parseErrorMessage (ParseError msg _) = msg

-- | Get the `Position` from a `ParseError`.
parseErrorPosition :: ParseError -> Position
parseErrorPosition (ParseError _ pos) = pos

instance showParseError :: Show ParseError where
  show (ParseError msg pos) =
    "(ParseError " <> show msg <> " " <> show pos <> ")"

derive instance eqParseError :: Eq ParseError
derive instance ordParseError :: Ord ParseError

-- | The internal state of the `ParserT s m` monad.
-- |
-- | Contains the remaining input and current position and the consumed flag.
-- |
-- | The consumed flag is used to implement the rule for `alt` that
-- | - If the left parser fails *without consuming any input*, then backtrack and try the right parser.
-- | - If the left parser fails and consumes input, then fail immediately.
data ParseState s = ParseState s Position Boolean
-- ParseState constructor has three parameters,
-- s: the remaining input
-- Position: the current position
-- Boolean: the consumed flag.
--
-- The consumed flag is used to implement the rule for `alt` that
-- * If the left parser fails *without consuming any input*, then backtrack and try the right parser.
-- * If the left parser fails and consumes input, then fail immediately.
--
-- https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html#v:try
--
-- http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/

-- | The `Parser s` monad with a monad transformer parameter `m`.
newtype ParserT s m a = ParserT
  -- The parser is implemented using continuation-passing-style with uncurried
  -- functions. In addition to the usual error and success continuations, there
  -- are continuations for trampolining and lifting. Trampolining lets us retain
  -- stack safety, and an explicit continuation for lifting lets us only pay
  -- a transformer abstraction tax when it's actually used. Pure parsers which
  -- never call `lift` pay no additional runtime cost. Additionally, this
  -- approach lets us run a parser in terms of the base Monad's MonadRec instance,
  -- so when lift _is_ used, it's still always stack safe.

  -- When should the trampoline be invoked? Downstream combinators should not need
  -- to worry about invoking the trampoline, as it's handled by the core instances
  -- of the parser (the Monad and Alternative hierarchies). These instances invoke
  -- the trampoline before calling continuations, so each step in the parser will
  -- always progress in a fresh stack.
  ( forall r
     . Fn5
         (ParseState s) -- Current state
         ((Unit -> r) -> r) -- Trampoline
         (m (Unit -> r) -> r) -- Lift
         (Fn2 (ParseState s) ParseError r) -- Throw
         (Fn2 (ParseState s) a r) -- Done/Success
         r
  )

-- When we want to run a parser, continuations are reified as data
-- constructors and processed in a tail-recursive loop.
data RunParser s m a
  = More (Unit -> RunParser s m a)
  | Lift (m (Unit -> RunParser s m a))
  | Stop (ParseState s) (Either ParseError a)

-- | `runParser` with a monad transfomer parameter `m`.
runParserT :: forall m s a. MonadRec m => s -> ParserT s m a -> m (Either ParseError a)
runParserT s p = fst <$> runParserT' initialState p
  where
  initialState :: ParseState s
  initialState = ParseState s initialPos false

-- | Run a parser and produce either an error or the result of the parser
-- | along with the internal state of the parser when it finishes.
runParserT'
  :: forall m s a
   . MonadRec m
  => ParseState s
  -> ParserT s m a
  -> m (Tuple (Either ParseError a) (ParseState s))
runParserT' state1 (ParserT k1) =
  tailRecM go \_ ->
    runFn5 k1 state1 More Lift
      (mkFn2 \state2 err -> Stop state2 (Left err))
      (mkFn2 \state2 res -> Stop state2 (Right res))
  where
  go
    :: (Unit -> RunParser s m a)
    -> m (Step (Unit -> RunParser s m a) (Tuple (Either ParseError a) (ParseState s)))
  go step = case step unit of
    More next ->
      go next
    Lift m ->
      Loop <$> m
    Stop s res ->
      pure $ Done (Tuple res s)

-- | The `Parser s` monad, where `s` is the type of the input stream.
-- |
-- | A synonym for the `ParserT` monad transformer applied
-- | to the `Identity` monad.
type Parser s = ParserT s Identity

-- | Run a parser on an input stream `s` and produce either an error or the
-- | result `a` of the parser.
runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = unwrap <<< runParserT s

hoistParserT :: forall s m n a. (m ~> n) -> ParserT s m a -> ParserT s n a
hoistParserT f (ParserT k) = ParserT
  ( mkFn5 \state1 more lift throw done ->
      runFn5 k state1 more (lift <<< f) throw done
  )

-- | Change the underlying monad action `m` and result data type `a` in
-- | a `ParserT s m` monad action.
mapParserT
  :: forall b n s a m
   . MonadRec m
  => Functor n
  => ( m (Tuple (Either ParseError a) (ParseState s))
       -> n (Tuple (Either ParseError b) (ParseState s))
     )
  -> ParserT s m a
  -> ParserT s n b
mapParserT f p = ParserT
  ( mkFn5 \state1 _ lift throw done ->
      lift $ map
        ( \(Tuple res state2) _ ->
            case res of
              Left err ->
                runFn2 throw state2 err
              Right a ->
                runFn2 done state2 a
        )
        (f (runParserT' state1 p))
  )

instance Lazy (ParserT s m a) where
  defer f = ParserT
    ( mkFn5 \state1 more lift throw done -> do
        let (ParserT k1) = Lazy.force m
        runFn5 k1 state1 more lift throw done
    )
    where
    m = Lazy.defer f

instance Semigroup a => Semigroup (ParserT s m a) where
  append = lift2 (<>)

instance Monoid a => Monoid (ParserT s m a) where
  mempty = pure mempty

instance Functor (ParserT s m) where
  map f (ParserT k) = ParserT
    ( mkFn5 \state1 more lift throw done ->
        more \_ ->
          runFn5 k state1 more lift throw
            ( mkFn2 \state2 a ->
                more \_ ->
                  runFn2 done state2 (f a)
            )
    )

instance Apply (ParserT s m) where
  apply (ParserT k1) (ParserT k2) = ParserT
    ( mkFn5 \state1 more lift throw done ->
        more \_ ->
          runFn5 k1 state1 more lift throw
            ( mkFn2 \state2 f ->
                more \_ ->
                  runFn5 k2 state2 more lift throw
                    ( mkFn2 \state3 a ->
                        more \_ ->
                          runFn2 done state3 (f a)
                    )
            )
    )

instance Applicative (ParserT s m) where
  pure a = ParserT
    ( mkFn5 \state1 _ _ _ done ->
        runFn2 done state1 a
    )

instance Bind (ParserT s m) where
  bind (ParserT k1) next = ParserT
    ( mkFn5 \state1 more lift throw done ->
        more \_ ->
          runFn5 k1 state1 more lift throw
            ( mkFn2 \state2 a ->
                more \_ -> do
                  let (ParserT k2) = next a
                  runFn5 k2 state2 more lift throw done
            )
    )

instance Monad (ParserT s m)

instance MonadRec (ParserT s m) where
  tailRecM next initArg = ParserT
    ( mkFn5 \state1 more lift throw done -> do
        let
          -- In most cases, trampolining MonadRec is unnecessary since all the
          -- core semantics are trampolined. But given the case where a loop might
          -- otherwise be pure, we still want to guarantee stack usage so we use
          -- a "gas" accumulator to avoid bouncing too much.
          loop = mkFn3 \state2 arg gas -> do
            let (ParserT k1) = next arg
            runFn5 k1 state2 more lift throw
              ( mkFn2 \state3 step -> case step of
                  Loop nextArg ->
                    if gas == 0 then
                      more \_ ->
                        runFn3 loop state3 nextArg 30
                    else
                      runFn3 loop state3 nextArg (gas - 1)
                  Done res ->
                    runFn2 done state3 res
              )
        runFn3 loop state1 initArg 30
    )

instance (MonadState t m) => MonadState t (ParserT s m) where
  state k = lift (state k)

instance (MonadAsk r m) => MonadAsk r (ParserT s m) where
  ask = lift ask

instance (MonadReader r m) => MonadReader r (ParserT s m) where
  local f (ParserT k) = ParserT
    ( mkFn5 \state1 more lift throw done ->
        runFn5 k state1 more (lift <<< local f) throw done
    )

instance MonadThrow ParseError (ParserT s m) where
  throwError err = ParserT
    ( mkFn5 \state1 _ _ throw _ ->
        runFn2 throw state1 err
    )

instance MonadError ParseError (ParserT s m) where
  catchError (ParserT k1) next = ParserT
    ( mkFn5 \state1 more lift throw done ->
        more \_ ->
          runFn5 k1 state1 more lift
            ( mkFn2 \state2 err -> do
                let (ParserT k2) = next err
                runFn5 k2 state2 more lift throw done
            )
            done
    )

-- | The alternative `Alt` instance provides the `alt` combinator `<|>`.
-- |
-- | The expression `p_left <|> p_right` will first try the `p_left` parser and if that fails
-- | __and consumes no input__ then it will try the `p_right` parser.
-- |
-- | While we are parsing down the `p_left` branch we may reach a point where
-- | we know this is the correct branch, but we cannot parse further. At
-- | that point we want to fail the entire parse instead of trying the `p_right`
-- | branch.
-- |
-- | For example, consider this `fileParser` which can parse either an HTML
-- | file that begins with `<html>` or a shell script file that begins with `#!`.
-- |
-- | ```
-- | fileParser =
-- |   string "<html>" *> parseTheRestOfTheHtml
-- |   <|>
-- |   string "#!" *> parseTheRestOfTheScript
-- | ```
-- |
-- | If we read a file from disk and run this `fileParser` on it and the
-- | `string "<html>"` parser succeeds, then we know that the first branch
-- | is the correct branch, so we want to commit to the first branch.
-- | Even if the `parseTheRestOfTheHtml` parser fails
-- | we don’t want to try the second branch.
-- |
-- | To control the point at which we commit to the `p_left` branch
-- | use the `try` combinator and the `lookAhead` combinator and
-- | the `consume` function.
-- |
-- | The `alt` combinator works this way because it gives us good localized
-- | error messages while also allowing an efficient implementation. See
-- | [*Parsec: Direct Style Monadic Parser Combinators For The Real World*](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)
-- | section __2.3 Backtracking__.
instance Alt (ParserT s m) where
  alt (ParserT k1) (ParserT k2) = ParserT
    ( mkFn5 \state1@(ParseState input pos _) more lift throw done ->
        more \_ ->
          runFn5 k1 (ParseState input pos false) more lift
            ( mkFn2 \state2@(ParseState _ _ consumed) err ->
                more \_ ->
                  if consumed then
                    runFn2 throw state2 err
                  else
                    runFn5 k2 state1 more lift throw done
            )
            done
    )

instance Plus (ParserT s m) where
  empty = fail "No alternative"

instance Alternative (ParserT s m)

instance MonadPlus (ParserT s m)

instance MonadTrans (ParserT s) where
  lift m = ParserT
    ( mkFn5 \state1 _ lift' _ done ->
        lift' $ map (\a _ -> runFn2 done state1 a) m
    )

-- | Query and modify the `ParserT` internal state.
-- |
-- | Like the `state` member of `MonadState`.
stateParserT :: forall s m a. (ParseState s -> Tuple a (ParseState s)) -> ParserT s m a
stateParserT k = ParserT
  ( mkFn5 \state1 _ _ _ done -> do
      let (Tuple a state2) = k state1
      runFn2 done state2 a
  )

-- | Query the `ParserT` internal state.
-- |
-- | Like the `get` member of `MonadState`.
getParserT :: forall s m. ParserT s m (ParseState s)
getParserT = ParserT
  ( mkFn5 \state1 _ _ _ done -> runFn2 done state1 state1
  )

-- | Set the consumed flag.
-- |
-- | Setting the consumed flag means that we're committed to this parsing branch
-- | of an alternative (`<|>`), so that if this branch fails then we want to
-- | fail the entire parse instead of trying the other alternative.
consume :: forall s m. ParserT s m Unit
consume = stateParserT \(ParseState input pos _) -> Tuple unit (ParseState input pos true)

-- | Returns the current position in the stream.
position :: forall s m. ParserT s m Position
position = stateParserT \state1@(ParseState _ pos _) -> Tuple pos state1

-- | Fail with a message.
fail :: forall m s a. String -> ParserT s m a
fail message = failWithPosition message =<< position

-- | Fail with a message and a position.
failWithPosition :: forall m s a. String -> Position -> ParserT s m a
failWithPosition message pos = throwError (ParseError message pos)

-- | Contextualize parsing failures inside a region. If a parsing failure
-- | occurs, then the `ParseError` will be transformed by each containing
-- | `region` as the parser backs out the call stack.
-- |
-- | For example, here’s a helper function `inContext` which uses `region` to
-- | add some string context to the error messages.
-- |
-- | ```
-- | let
-- |   inContext :: forall s m a. (String -> String) -> ParserT s m a -> ParserT s m a
-- |   inContext context = region \(ParseError message pos) ->
-- |     ParseError (context message) pos
-- |
-- |   input = "Tokyo thirty-nine million"
-- |
-- | lmap (parseErrorHuman input 30) $ runParser input do
-- |   inContext ("Megacity list: " <> _) do
-- |     cityname <- inContext ("city name: " <> _) (takeWhile isLetter)
-- |     skipSpaces
-- |     population <- inContext ("population: " <> _) intDecimal
-- |     pure $ Tuple cityname population
-- | ```
-- | ---
-- | ```
-- | Megacity list: population: Expected Int at position index:6 (line:1, column:7)
-- |       ▼
-- | Tokyo thirty-nine million
-- | ```
region :: forall m s a. (ParseError -> ParseError) -> ParserT s m a -> ParserT s m a
region context p = catchError p $ \err -> throwError $ context err

-- | `Position` represents the position of the parser in the input stream.
-- |
-- | - `index` is the position offset since the start of the input. Starts
-- |   at *0*.
-- | - `line` is the current line in the input. Starts at *1*.
-- | - `column` is the column of the next character in the current line that
-- |   will be parsed. Starts at *1*.
newtype Position = Position
  { index :: Int
  , line :: Int
  , column :: Int
  }

derive instance Generic Position _
instance Show Position where
  show x = genericShow x

instance Eq Position where
  eq (Position l) (Position r) = l.index == r.index

instance Ord Position where
  compare (Position l) (Position r) = compare l.index r.index

-- | The `Position` before any input has been parsed.
-- |
-- | `{ index: 0, line: 1, column: 1 }`
initialPos :: Position
initialPos = Position { index: 0, line: 1, column: 1 }

-- | Lift a `Maybe a` computation into a `ParserT`, with a note for
-- | the `ParseError` message in case of `Nothing`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `Maybe` computation is `Nothing`, then this will `fail` in the
-- | `ParserT` monad with the given error message `String` at the current input
-- | `Position`.
-- |
-- | This is a “validation” function, for when we want to produce some
-- | data from the parsing input or fail at the current
-- | parsing position if that’s impossible.
-- |
-- | For example, parse an integer
-- | [`BoundedEnum`](https://pursuit.purescript.org/packages/purescript-enums/docs/Data.Enum#t:BoundedEnum)
-- | code and validate it by turning it
-- | into a `MyEnum`. Use `tryRethrow` to position the parse error at the
-- | beginning of the integer in the input `String` if the `toEnum` fails.
-- |
-- | ```
-- | runParser "3" do
-- |   myenum :: MyEnum <- tryRethrow do
-- |     x <- intDecimal
-- |     liftMaybe (\_ -> "Bad MyEnum " <> show x) $ toEnum x
-- | ```
liftMaybe :: forall s m a. Monad m => (Unit -> String) -> Maybe a -> ParserT s m a
liftMaybe message f = case f of
  Nothing -> fail (message unit)
  Just x -> pure x

-- | Lift an `Either String a` computation into a `ParserT`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `Either` computation is `Left String`, then this will `fail` in the
-- | `ParserT` monad at the current input `Position`.
-- |
-- | This is a “validation” function, for when we want to produce some
-- | data from the parsing input or fail at the current
-- | parsing position if that’s impossible.
liftEither :: forall s m a. Monad m => Either String a -> ParserT s m a
liftEither f = case f of
  Left err -> fail err
  Right x -> pure x

-- | Lift an `ExceptT String m a` computation into a `ParserT`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `ExceptT` computation is `Left String`, then this will `fail` in the
-- | `ParserT` monad at the current input `Position`.
-- |
-- | This is a “validation” function, for when we want to produce some
-- | data from the parsing input or fail at the current
-- | parsing position if that’s impossible.
liftExceptT :: forall s m a. (Monad m) => ExceptT String m a -> ParserT s m a
liftExceptT f = lift (runExceptT f) >>= case _ of
  Left err -> fail err
  Right x -> pure x
