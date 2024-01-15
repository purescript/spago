-- The majority of this code was lifted from
-- - https://github.com/nwolverson/purescript-suggest
--
-- Original license is MIT:
--   Copyright © Nicholas Wolverson
--   https://opensource.org/license/mit/
module Spago.Psa.Suggest (listSuggestions, applySuggestions) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.String as Str
import Data.String.Regex (regex, test, replace) as Regex
import Data.String.Regex.Flags (noFlags, global) as Regex
import Dodo as Log
import Effect.Ref as Ref
import Spago.FS as FS
import Spago.Psa.Output as Psa.Output
import Spago.Psa.Types (Position, PsaError, PsaPath(..))
import Spago.Psa.Types as Psa.Types

--------------------------------------------------------------------------------
-- Entrypoints

applySuggestions :: Array PsaError -> Spago (LogEnv _) Unit
applySuggestions warnings = do
  { replacements, files } <- getSuggestions warnings
  for_ replacements $ \group ->
    case Array.head group of
      Just { filename } -> do
        logInfo $ "Applying suggestions to " <> filename
        liftEffect (Ref.read files) >>= \res -> case Map.lookup filename res of
          Just lines -> replaceFile lines filename group
          _ -> pure unit
      Nothing -> pure unit

listSuggestions :: Array PsaError -> Spago (LogEnv _) (Array Docc)
listSuggestions warnings = do
  { replacements } <- getSuggestions warnings
  let totalCount = Array.length (Array.concat replacements)
  pure
    $ [ Log.break, toDoc $ "There are " <> show totalCount <> " warnings that could be fixed automatically:" ]
    <> map (indent2 <<< toDoc) (Array.mapMaybe rep replacements)
    <> [ Log.break, toDoc "Run `spago build --autofix-warnings` to apply them", Log.break ]
  where
  rep reps = case Array.head reps of
    Just { filename } -> Just $ filename <> ": " <> show (Array.length reps) <> " replacements"
    _ -> Nothing

--------------------------------------------------------------------------------
-- Implementation

type Replacement =
  { filename :: String
  , position :: Position
  , original :: String
  , replacement :: String
  }

type Suggestions = { replacements :: Array (Array Replacement), files :: Ref (Map String (Array String)) }

getSuggestions :: Array PsaError -> Spago (LogEnv _) Suggestions
getSuggestions warnings = do
  files <- liftEffect $ Ref.new Map.empty
  let loadLinesImpl = loadLines files
  warnings' <- Array.sortBy Psa.Types.compareByLocation <$> Array.catMaybes <$> traverse (annotateError loadLinesImpl) warnings
  let replacements = Array.mapMaybe getReplacement warnings'
  pure { replacements: map NEA.toArray (Array.groupBy (\a b -> a.filename == b.filename) replacements), files }
  where
  loadLines files filename pos = do
    contents <- (liftEffect $ Ref.read files) >>= \cache ->
      case Map.lookup filename cache of
        Just lines -> pure lines
        Nothing -> do
          lines <- Str.split (Str.Pattern "\n") <$> FS.readTextFile filename
          liftEffect $ Ref.modify_ (Map.insert filename lines) files
          pure lines
    let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
    pure $ Just source

  getReplacement { source: Just s, error: { suggestion: Just { replacement, replaceRange }, filename: Just filename }, position: Just position } =
    Just
      { filename
      , position: fromMaybe position replaceRange
      , original: Str.joinWith "\n" s
      , replacement
      }
  getReplacement _ = Nothing

  annotateError loadLinesF error = do
    source <- fromMaybe (pure Nothing) (loadLinesF <$> error.filename <*> error.position)
    pure $ Psa.Output.annotatedError <$> (Src <$> error.filename) <*> pure source <*> pure error

replaceFile :: Array String -> String -> Array Replacement -> Spago (LogEnv _) Unit
replaceFile lines filename group =
  case replaceFile' 1 1 (List.fromFoldable lines) (List.fromFoldable group) of
    Left err -> logDebug err
    Right outLines -> do
      FS.writeTextFile filename $ List.intercalate "" outLines
      logInfo $ filename <> ": Applied " <> show (Array.length group) <> " fixes"

-- | This is where all the real work happens.
-- | Steps through the source file, outputting replacement text when the position
-- | matches otherwise the original text. Objects if replacements overlap or go past the file end.
replaceFile' :: Int -> Int -> List String -> List Replacement -> Either String (List String)
replaceFile' n _ lines reps@(Cons { position: { startLine } } _) | n < startLine && List.length lines >= startLine - n =
  (withNewlines (List.take count lines) <> _) <$> replaceFile' startLine 1 (List.drop count lines) reps
  where
  count = startLine - n
replaceFile' n m lines (Cons { position: { startLine, startColumn, endLine, endColumn }, replacement } reps) | n == startLine =
  let
    initial = Str.take (startColumn - m) (fromMaybe "" $ List.head lines)
    final = Str.drop (endColumn - (if startLine == endLine then m else 1)) (fromMaybe "" $ List.index lines (endLine - startLine))
    trailingNewline = either (const true) (\regex -> Regex.test regex replacement) (Regex.regex "\n\\s+$" Regex.noFlags)
    addNewline = trailingNewline && (not $ Str.null final)
    replace regex s text = either (const text) (\regex' -> Regex.replace regex' s text) (Regex.regex regex Regex.global)
    tweak = replace "\\n(.)" ("\n" <> List.fold (Array.replicate (startColumn - 1) " ") <> "$1")
      >>> replace "\\s+\\n" "\n"
      >>>
        Str.trim
    newText = initial <> tweak replacement <> (if addNewline then "\n" else "")
    replaceNewText = case newText of
      "" -> identity
      _ -> Cons newText
    remainingLines = (List.drop (endLine - startLine + 1) lines)
  in
    if final == "" && newText == "" then
      -- Avoid blank lines when replacing entire line(s) with blank
      replaceNewText <$> replaceFile' (endLine + 1) 1 remainingLines reps
    else
      replaceNewText <$> replaceFile' endLine endColumn (Cons final remainingLines) reps

replaceFile' n _ _ (Cons { position: { startLine } } _) | n > startLine =
  Left $ "Found replacement starting before current position: " <> show startLine <> ", " <> show n
replaceFile' _ _ lines Nil = pure $ List.intercalate (Cons "\n" Nil) (List.singleton <$> lines)
replaceFile' _ _ _ _ = Left "Found replacement after end of file"

withNewlines :: List String -> List String
withNewlines = List.concatMap (\x -> Cons x (Cons "\n" Nil))
