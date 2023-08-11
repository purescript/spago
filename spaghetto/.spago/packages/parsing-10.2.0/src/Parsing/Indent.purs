-- | This module is a port of the Haskell
-- | [__Text.Parsec.Indent__](https://hackage.haskell.org/package/indents-0.3.3/docs/Text-Parsec-Indent.html)
-- | module from 2016-05-07.
-- |
-- | A module to construct indentation aware parsers. Many programming
-- | language have indentation based syntax rules e.g. python and Haskell.
-- | This module exports combinators to create such parsers.
-- |
-- | The input source can be thought of as a list of tokens. Abstractly
-- | each token occurs at a line and a column and has a width. The column
-- | number of a token measures is indentation. If t1 and t2 are two tokens
-- | then we say that indentation of t1 is more than t2 if the column
-- | number of occurrence of t1 is greater than that of t2.
-- |
-- | Currently this module supports two kind of indentation based syntactic
-- | structures which we now describe:
-- |
-- | - **Block**
-- |
-- |   A block of indentation /c/ is a sequence of tokens with
-- |   indentation at least /c/.  Examples for a block is a where clause of
-- |   Haskell with no explicit braces.
-- |
-- | - **Line fold**
-- |
-- |   A line fold starting at line /l/ and indentation /c/ is a
-- |   sequence of tokens that start at line /l/ and possibly continue to
-- |   subsequent lines as long as the indentation is greater than /c/. Such
-- |   a sequence of lines need to be /folded/ to a single line. An example
-- |   is MIME headers. Line folding based binding separation is used in
-- |   Haskell as well.
module Parsing.Indent
  ( IndentParser
  , runIndent
  , withBlock
  , withBlock'
  , block
  , block1
  , indented
  , indented'
  , sameLine
  , sameOrIndented
  , checkIndent
  , withPos
  , indentAp
  , (<+/>)
  , indentNoAp
  , (<-/>)
  , indentMany
  , (<*/>)
  , indentOp
  , (<?/>)
  , indentBrackets
  , indentAngles
  , indentBraces
  , indentParens
  , Optional(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (get, put)
import Control.Monad.Trans.Class (lift)
import Data.List (List(..), many)
import Data.Maybe (Maybe(..))
import Parsing (ParserT, fail, position, Position(..), initialPos)
import Parsing.Combinators (option, optionMaybe)
import Parsing.String (string)
import Parsing.String.Basic (oneOf)

-- | Indentation sensitive parser type. Usually @ m @ will
-- | be @ Identity @ as with any @ ParserT @
type IndentParser s a = ParserT s (State Position) a

-- | simple helper function to avoid typ-problems with MonadState instance
get' :: forall s. IndentParser s Position
get' = do
  g <- lift get
  pure g

-- | simple helper function to avoid typ-problems with MonadState instance
put' :: forall s. Position -> IndentParser s Unit
put' p = lift (put p)

many1 :: forall s m a. ParserT s m a -> ParserT s m (List a)
many1 p = lift2 Cons p (many p)

symbol :: forall m. String -> ParserT String m String
symbol name = (many $ oneOf [ ' ', '\t' ]) *> (string name)

-- | `withBlock f a p` parses `a`
-- | followed by an indented block of `p`
-- | combining them with `f`.
withBlock :: forall a b c s. (a -> List b -> c) -> IndentParser s a -> IndentParser s b -> IndentParser s c
withBlock f a p = withPos $ do
  r1 <- a
  r <- optionMaybe $ indented *> block p
  case r of
    Nothing -> pure (f r1 Nil)
    Just r2 -> pure (f r1 r2)

-- | Like 'withBlock', but throws away initial parse result
withBlock' :: forall a b s. IndentParser s a -> IndentParser s b -> IndentParser s (List b)
withBlock' = withBlock (flip const)

-- | Parses only when indented past the level of the reference
indented :: forall s. IndentParser s Unit
indented = do
  Position p <- position
  Position s <- get'
  if p.column <= s.column then fail "not indented"
  else put' $ Position { index: 0, line: p.line, column: s.column }

-- | Same as `indented`, but does not change internal state
indented' :: forall s. IndentParser s Unit
indented' = do
  Position p <- position
  Position s <- get'
  if p.column <= s.column then fail "not indented" else pure unit

-- | Parses only when indented past the level of the reference or on the same line
sameOrIndented :: forall s. IndentParser s Unit
sameOrIndented = sameLine <|> indented

-- | Parses only on the same line as the reference
sameLine :: forall s. IndentParser s Unit
sameLine = do
  Position p <- position
  Position s <- get'
  if p.line == s.line then pure unit else fail "over one line"

-- | Parses a block of lines at the same indentation level
block1 :: forall s a. IndentParser s a -> IndentParser s (List a)
block1 p = withPos $ do
  r <- many1 $ checkIndent *> p
  pure r

-- | Parses a block of lines at the same indentation level , empty Blocks allowed
block :: forall s a. IndentParser s a -> IndentParser s (List a)
block p = withPos $ do
  r <- many $ checkIndent *> p
  pure r

-- | Parses using the current location for indentation reference
withPos :: forall s a. IndentParser s a -> IndentParser s a
withPos x = do
  a <- get'
  p <- position
  r <- put' p *> x
  put' a *> pure r

-- | Ensures the current indentation level matches that of the reference
checkIndent :: forall s. IndentParser s Unit
checkIndent = do
  Position p <- position
  Position s <- get'
  if p.column == s.column then pure unit else fail "indentation doesn't match"

-- | Run the result of an indentation sensitive parse
runIndent :: forall a. State Position a -> a
runIndent = flip evalState initialPos

-- | `<+/>` is to indentation sensitive parsers what `ap` is to monads
indentAp :: forall s a b. IndentParser s (a -> b) -> IndentParser s a -> IndentParser s b
indentAp a b = ap a $ sameOrIndented *> b

infixl 9 indentAp as <+/>

-- | Like `<+/>` but doesn't apply the function to the parsed value
indentNoAp :: forall s a b. IndentParser s a -> IndentParser s b -> IndentParser s a
indentNoAp a b = lift2 const a $ sameOrIndented *> b

infixl 10 indentNoAp as <-/>

-- | Like `<+/>` but applies the second parser many times
indentMany :: forall s a b. IndentParser s (List a -> b) -> IndentParser s a -> IndentParser s b
indentMany a b = ap a (many (sameOrIndented *> b))

infixl 11 indentMany as <*/>

-- | Data type used to optional parsing
data Optional s a = Opt a (IndentParser s a)

-- | Like `<+/>` but applies the second parser optionally using the `Optional` datatype
indentOp :: forall s a b. IndentParser s (a -> b) -> Optional s a -> IndentParser s b
indentOp a (Opt b c) = ap a (option b (sameOrIndented *> c))

infixl 12 indentOp as <?/>

-- | Parses with surrounding brackets
indentBrackets :: forall a. IndentParser String a -> IndentParser String a
indentBrackets p = withPos $ pure identity <-/> symbol "[" <+/> p <-/> symbol "]"

-- | Parses with surrounding angle brackets
indentAngles :: forall a. IndentParser String a -> IndentParser String a
indentAngles p = withPos $ pure identity <-/> symbol "<" <+/> p <-/> symbol ">"

-- | Parses with surrounding braces
indentBraces :: forall a. IndentParser String a -> IndentParser String a
indentBraces p = withPos $ pure identity <-/> symbol "{" <+/> p <-/> symbol "}"

-- | Parses with surrounding parentheses
indentParens :: forall a. IndentParser String a -> IndentParser String a
indentParens p = withPos $ pure identity <-/> symbol "(" <+/> p <-/> symbol ")"
