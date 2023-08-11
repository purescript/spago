module PureScript.CST.Parser.Monad
  ( Parser
  , ParserState
  , ParserResult(..)
  , PositionedError
  , initialParserState
  , fromParserResult
  , runParser
  , runParser'
  , take
  , fail
  , try
  , lookAhead
  , many
  , optional
  , eof
  , recover
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn4, mkFn2, mkFn4, runFn2, runFn4)
import Data.Lazy as Lazy
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Comment, LineFeed, SourcePos, SourceToken)

type PositionedError =
  { position :: SourcePos
  , error :: ParseError
  }

type ParserState =
  { consumed :: Boolean
  , errors :: Array PositionedError
  , stream :: TokenStream
  }

initialParserState :: TokenStream -> ParserState
initialParserState stream =
  { consumed: false
  , errors: []
  , stream
  }

newtype Parser a = Parser
  ( forall r
     . Fn4
         ParserState
         ((Unit -> r) -> r)
         (Fn2 ParserState PositionedError r)
         (Fn2 ParserState a r)
         r
  )

instance Functor Parser where
  map f (Parser p) = Parser
    ( mkFn4 \state1 more resume done ->
        runFn4 p state1 more resume
          ( mkFn2 \state2 a ->
              runFn2 done state2 (f a)
          )
    )

instance Apply Parser where
  apply (Parser p1) (Parser p2) = Parser
    ( mkFn4 \state1 more resume done ->
        runFn4 p1 state1 more resume
          ( mkFn2 \state2 f ->
              more \_ ->
                runFn4 p2 state2 more resume
                  ( mkFn2 \state3 a ->
                      runFn2 done state3 (f a)
                  )
          )
    )

instance Applicative Parser where
  pure a = Parser
    ( mkFn4 \state1 _ _ done ->
        runFn2 done state1 a
    )

instance Bind Parser where
  bind (Parser p1) k = Parser
    ( mkFn4 \state1 more resume done ->
        runFn4 p1 state1 more resume
          ( mkFn2 \state2 a ->
              more \_ -> do
                let (Parser p2) = k a
                runFn4 p2 state2 more resume done
          )
    )

instance Monad Parser

instance Alt Parser where
  alt (Parser k1) (Parser k2) = Parser
    ( mkFn4 \state1 more resume done -> do
        let
          state2 =
            if state1.consumed then state1 { consumed = false }
            else state1
        runFn4 k1 state2 more
          ( mkFn2 \state3 error ->
              if state3.consumed then
                runFn2 resume state3 error
              else
                runFn4 k2 state1 more resume done
          )
          done
    )

instance Lazy (Parser a) where
  defer k = Parser
    ( mkFn4 \state more resume done -> do
        let (Parser k) = Lazy.force parser
        runFn4 k state more resume done
    )
    where
    parser = Lazy.defer k

fail :: forall a. PositionedError -> Parser a
fail error = Parser (mkFn4 \state _ resume _ -> runFn2 resume state error)

try :: forall a. Parser a -> Parser a
try (Parser p) = Parser
  ( mkFn4 \state1 more resume done ->
      runFn4 p state1 more
        ( mkFn2 \state2 error ->
            runFn2 resume (state2 { consumed = state1.consumed }) error
        )
        done
  )

recover :: forall a. (PositionedError -> TokenStream -> Maybe (Tuple a TokenStream)) -> Parser a -> Parser a
recover k (Parser p) = Parser
  ( mkFn4 \state1 more resume done -> do
      runFn4 p (state1 { consumed = false }) more
        ( mkFn2 \state2 error ->
            case k error state1.stream of
              Nothing ->
                runFn2 resume (state2 { consumed = state1.consumed }) error
              Just (Tuple a stream) ->
                runFn2 done
                  { consumed: true
                  , errors: Array.snoc state2.errors error
                  , stream
                  }
                  a
        )
        done
  )

take :: forall a. (SourceToken -> Either ParseError a) -> Parser a
take k = Parser
  ( mkFn4 \state _ resume done ->
      case TokenStream.step state.stream of
        TokenError position error _ _ ->
          runFn2 resume state { error, position }
        TokenEOF position _ ->
          runFn2 resume state { error: UnexpectedEof, position }
        TokenCons tok _ nextStream _ ->
          case k tok of
            Left error ->
              runFn2 resume state { error, position: tok.range.start }
            Right a ->
              runFn2 done
                ( state
                    { consumed = true
                    , stream = nextStream
                    }
                )
                a
  )

eof :: Parser (Tuple SourcePos (Array (Comment LineFeed)))
eof = Parser
  ( mkFn4 \state _ resume done ->
      case TokenStream.step state.stream of
        TokenError position error _ _ ->
          runFn2 resume state { error, position }
        TokenEOF position comments ->
          runFn2 done (state { consumed = true }) (Tuple position comments)
        TokenCons tok _ _ _ ->
          runFn2 resume state
            { error: ExpectedEof tok.value
            , position: tok.range.start
            }
  )

lookAhead :: forall a. Parser a -> Parser a
lookAhead (Parser p) = Parser
  ( mkFn4 \state1 more resume done ->
      runFn4 p state1 more
        (mkFn2 \_ error -> runFn2 resume state1 error)
        (mkFn2 \_ value -> runFn2 done state1 value)
  )

many :: forall a. Parser a -> Parser (Array a)
many (Parser p) = Parser
  ( mkFn4 \state1 more resume done -> do
      let
        go = mkFn2 \acc state2 -> do
          let
            state2' =
              if state2.consumed then state2 { consumed = false }
              else state2
          runFn4 p state2' more
            ( mkFn2 \state3 error ->
                if state3.consumed then
                  runFn2 resume state3 error
                else
                  runFn2 done state2 (Array.reverse (List.toUnfoldable acc))
            )
            ( mkFn2 \state3 value ->
                runFn2 go (List.Cons value acc) state3
            )
      runFn2 go List.Nil state1
  )

optional :: forall a. Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

data Trampoline a = More (Unit -> Trampoline a) | Done a

runParser' :: forall a. ParserState -> Parser a -> ParserResult a
runParser' state1 (Parser p) =
  run $ runFn4 p state1 More
    (mkFn2 \state2 error -> Done (ParseFail error state2))
    (mkFn2 \state2 value -> Done (ParseSucc value state2))
  where
  run = case _ of
    More k -> run (k unit)
    Done a -> a

runParser :: forall a. TokenStream -> Parser a -> Either PositionedError (Tuple a (Array PositionedError))
runParser stream = fromParserResult <<< runParser' (initialParserState stream)

data ParserResult a
  = ParseFail PositionedError ParserState
  | ParseSucc a ParserState

fromParserResult :: forall a. ParserResult a -> Either PositionedError (Tuple a (Array PositionedError))
fromParserResult = case _ of
  ParseFail error _ ->
    Left error
  ParseSucc res { errors } ->
    Right (Tuple res errors)
