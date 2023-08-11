module Options.Applicative.Builder.Completer
  ( module Reexport
  , listIOCompleter
  , listCompleter
  , bashCompleter
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array (elem)
import Data.Foldable (foldr)
import Data.Array as Array
import Data.Either (either)
import Data.List as List
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Options.Applicative.Internal.Utils (lines, startsWith, unWords)
import Options.Applicative.Types (Completer(..))
import Options.Applicative.Types (Completer, mkCompleter) as Reexport
-- import System.Process (readProcess)

-- | Create a 'Completer' from an IO action
listIOCompleter :: Effect (Array String) -> Completer
listIOCompleter ss = Completer $ \s ->
  Array.filter (startsWith $ String.Pattern s) <$> ss

-- | Create a 'Completer' from a constant
-- list of strings.
listCompleter :: (Array String) -> Completer
listCompleter = listIOCompleter <<< pure

-- | Run a compgen completion action.
--
-- Common actions include @file@ and
-- @directory@. See
-- <http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins>
-- for a complete list.
bashCompleter :: String -> Completer
bashCompleter action = Completer $ \word -> do
  let cmd = unWords ["compgen", "-A", action, "--", requote word]
  result <- try $ execSyncCommand $ "bash -c " <> cmd
  pure <<< lines <<< either (const "") identity $ result

foreign import execSyncCommand :: String -> Effect String

-- | Strongly quote the string we pass to compgen.
--
-- We need to do this so bash doesn't expand out any ~ or other
-- chars we want to complete on, or emit an end of line error
-- when seeking the close to the quote.
requote :: String -> String
requote = toCharArray >>> List.fromFoldable >>> go' >>> List.toUnfoldable >>> fromCharArray
  where
  go' :: List.List Char -> List.List Char
  go' = \s ->
    let
      -- Bash doesn't appear to allow "mixed" escaping
      -- in bash completions. So we don't have to really
      -- worry about people swapping between strong and
      -- weak quotes.
      unescaped =
        case s of
          -- It's already strongly quoted, so we
          -- can use it mostly as is, but we must
          -- ensure it's closed off at the end and
          -- there's no single quotes in the
          -- middle which might confuse bash.
          ('\'' List.: rs) -> unescapeN rs

          -- We're weakly quoted.
          ('"' List.: rs)  -> unescapeD rs

          -- We're not quoted at all.
          -- We need to unescape some characters like
          -- spaces and quotation marks.
          elsewise   -> unescapeU elsewise
    in
      strong unescaped

    where
      strong :: List.List Char -> List.List Char
      strong ss = '\'' List.: foldr go (List.singleton '\'') ss
        where
          -- If there's a single quote inside the
          -- command: exit from the strong quote and
          -- emit it the quote escaped, then resume.
          go '\'' t = '\'' List.: '\\' List.: '\'' List.: t
          go h t    = h List.: t

      -- Unescape a strongly quoted string
      -- We have two recursive functions, as we
      -- can enter and exit the strong escaping.
      unescapeN :: List.List Char -> List.List Char
      unescapeN = goX
        where
          goX ('\'' List.: xs) = goN xs
          goX (x List.: xs) = x List.: goX xs
          goX List.Nil = List.Nil

          goN ('\\' List.: '\'' List.: xs) = '\'' List.: goN xs
          goN ('\'' List.: xs) = goX xs
          goN (x List.: xs) = x List.: goN xs
          goN List.Nil = List.Nil

      -- Unescape an unquoted string
      unescapeU :: List.List Char -> List.List Char
      unescapeU = goX
        where
          goX List.Nil = List.Nil
          goX ('\\' List.: x List.: xs) = x List.: goX xs
          goX (x List.: xs) = x List.: goX xs

      -- Unescape a weakly quoted string
      unescapeD :: List.List Char -> List.List Char
      unescapeD = goX
        where
          -- Reached an escape character
          goX ('\\' List.: x List.: xs)
            -- If it's true escapable, strip the
            -- slashes, as we're going to strong
            -- escape instead.
            | x `elem` ['$', '`', '\"', '\\', '\n'] = x List.: goX xs
            | otherwise = '\\' List.: x List.: goX xs
          -- We've ended quoted section, so we
          -- don't recurse on goX, it's done.
          goX ('"' List.: xs)
            = xs
          -- Not done, but not a special character
          -- just continue the fold.
          goX (x List.: xs)
            = x List.: goX xs
          goX List.Nil
            = List.Nil
