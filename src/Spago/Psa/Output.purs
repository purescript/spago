-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Output
  ( buildOutput
  , Output
  , OutputStats
  , annotatedError
  , trimPosition
  , trimMessage
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as Str
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Node.Path as Path
import Spago.Core.Config (CensorBuildWarnings(..))
import Spago.Core.Config as Core
import Spago.Core.Prelude (Spago)
import Spago.Paths as Paths
import Spago.Psa.Types (PsaOutputOptions, PsaError, PsaAnnotedError, PsaPath(..), PsaResult, Position, Filename, Lines, compareByLocation)
import Spago.Purs as Purs

data ErrorTag = Error | Warning

type Output =
  { warnings :: Array PsaAnnotedError
  , errors :: Array PsaAnnotedError
  , stats :: OutputStats
  }

-- | Statistics are a ratio of errors shown to errors in total.
type OutputStats =
  { allWarnings :: FO.Object (Tuple Int Int)
  , allErrors :: FO.Object (Tuple Int Int)
  , srcWarnings :: FO.Object (Tuple Int Int)
  , srcErrors :: FO.Object (Tuple Int Int)
  , libWarnings :: FO.Object (Tuple Int Int)
  , libErrors :: FO.Object (Tuple Int Int)
  }

initialStats :: OutputStats
initialStats =
  { allWarnings: FO.empty
  , allErrors: FO.empty
  , srcWarnings: FO.empty
  , srcErrors: FO.empty
  , libWarnings: FO.empty
  , libErrors: FO.empty
  }

-- | Annotates a error/warning result set with original source lines, better
-- | positions, and semantic paths (lib vs src). The callback should load the
-- | requested set of lines from the absolute filename based on the tentative
-- | position information.
buildOutput
  :: forall a
   . (Filename -> Position -> Spago (Purs.PursEnv a) (Maybe Lines))
  -> PsaOutputOptions
  -> PsaResult
  -> Spago (Purs.PursEnv a) Output
buildOutput loadLines options result = do
  let
    result' =
      { warnings: pathOf <$> result.warnings
      , errors: pathOf <$> result.errors
      }
    initialState =
      { warnings: []
      , errors: []
      , stats: initialStats
      }
  state <- Array.foldM (onError Warning) initialState result'.warnings
  state' <- Array.foldM (onError Error) state result'.errors
  pure state'
    { warnings = Array.sortBy compareByLocation state'.warnings
    , errors = Array.sortBy compareByLocation state'.errors
    }

  where
  pathOf :: PsaError -> Tuple PsaPath PsaError
  pathOf x =
    case x.filename of
      Just f | f /= "" ->
        let
          path
            | Path.isAbsolute f = f
            | otherwise = Path.concat [ Paths.cwd, f ]
        in
          Tuple (errorPath options.libraryDirs path f) x
      _ -> Tuple Unknown x

  onError :: ErrorTag -> Output -> Tuple PsaPath PsaError -> Spago (Purs.PursEnv a) Output
  onError tag state (Tuple path error) =
    if shouldShowError options tag path error.errorCode then do
      source <- fromMaybe (pure Nothing) (loadLines <$> error.filename <*> error.position)
      update [ annotatedError path source error ]
    else
      update []

    where
    update :: Array PsaAnnotedError -> Spago (Purs.PursEnv a) Output
    update log =
      pure $ onTag
        (_ { stats = stats, errors = state.errors <> log })
        (_ { stats = stats, warnings = state.warnings <> log })
        tag'
        state
      where
      printed = not (Array.null log)
      tag' = if printed && options.strict && isSrc path then Error else tag
      stats = updateStats tag' path error.errorCode printed state.stats

annotatedError :: PsaPath -> Maybe Lines -> PsaError -> PsaAnnotedError
annotatedError path lines error = { path, position, message, source, error }
  where
  position = trimPosition <$> lines <*> error.position
  message = trimMessage error.message
  source = (\p -> Array.take (p.endLine - p.startLine + 1)) <$> position <*> lines

updateStats
  :: ErrorTag
  -> PsaPath
  -> String
  -> Boolean -- If the error was printed
  -> OutputStats
  -> OutputStats
updateStats tag path code printed s =
  { allWarnings: onTag identity bumpCode tag s.allWarnings
  , allErrors: onTag bumpCode identity tag s.allErrors
  , srcWarnings: onTag identity (onPath bumpCode identity path) tag s.srcWarnings
  , srcErrors: onTag (onPath bumpCode identity path) identity tag s.srcErrors
  , libWarnings: onTag identity (onPath identity bumpCode path) tag s.libWarnings
  , libErrors: onTag (onPath identity bumpCode path) identity tag s.libErrors
  }

  where
  bumpCode = FO.alter alterStat code
  bump (Tuple a b) = Tuple (if printed then a + 1 else a) (b + 1)
  alterStat Nothing = Just (bump (Tuple 0 0))
  alterStat (Just x) = Just (bump x)

censorSrc :: Core.CensorBuildWarnings -> Boolean
censorSrc = case _ of
  CensorAllWarnings -> true
  CensorProjectWarnings -> true
  _ -> false

censorLib :: Core.CensorBuildWarnings -> Boolean
censorLib = case _ of
  CensorAllWarnings -> true
  CensorProjectWarnings -> true
  _ -> false

shouldShowError :: PsaOutputOptions -> ErrorTag -> PsaPath -> String -> Boolean
shouldShowError _ Error _ _ = true
shouldShowError { filterCodes, censorCodes, censorBuildWarnings } _ path code =
  not (censorSrc censorBuildWarnings && isSrc path || censorLib censorBuildWarnings && isLib path)
    && (Set.isEmpty filterCodes || Set.member code filterCodes)
    && (Set.isEmpty censorCodes || not (Set.member code censorCodes))

errorPath :: Array String -> String -> String -> PsaPath
errorPath libDirs path short =
  if any (\dir -> path `startsWith` Str.Pattern dir) libDirs then Lib short
  else Src short
  where
  startsWith s' s =
    case Str.indexOf s s' of
      Just 0 -> true
      _ -> false

onTag :: forall a b. (a -> b) -> (a -> b) -> ErrorTag -> a -> b
onTag f _ Error x = f x
onTag _ g Warning x = g x

onPath :: forall a. (a -> a) -> (a -> a) -> PsaPath -> a -> a
onPath f _ (Src _) x = f x
onPath _ g (Lib _) x = g x
onPath _ _ _ x = x

isLib :: PsaPath -> Boolean
isLib (Lib _) = true
isLib _ = false

isSrc :: PsaPath -> Boolean
isSrc (Src _) = true
isSrc _ = false

-- | Finds the true bounds of the source. The PureScript compiler is greedy
-- | when it comes to matching whitespace at the end of an expression, so the
-- | original source bounds always includes whitespace and comments.
trimPosition :: Lines -> Position -> Position
trimPosition lines pos =
  case lines of
    [] ->
      { startLine: pos.startLine
      , startColumn: pos.startColumn
      , endLine: pos.startLine
      , endColumn: pos.startColumn
      }

    [ l ] ->
      case trimCol (pos.endColumn) l of
        Just col -> pos { endLine = pos.startLine, endColumn = col }
        Nothing -> pos { endLine = pos.startLine, endColumn = pos.startColumn }

    _ ->
      case trimPos { row: pos.endLine, col: pos.endColumn } of
        Just { row, col } -> pos { endLine = row, endColumn = col }
        Nothing -> trimPosition [] pos

  where
  -- WARNING: here be off-by-one dragons!

  trimPos { row, col }
    | col <= 1 =
        case Array.index lines (row - pos.startLine - 1) of
          Just l -> trimPos { row: row - 1, col: Str.length l + 1 }
          _ -> Nothing

    | otherwise =
        case Array.index lines (row - pos.startLine) of
          Just l ->
            case trimCol col l of
              Just col' -> Just { row, col: col' }
              Nothing -> trimPos { row, col: 1 }

          _ -> Nothing

  trimCol col l =
    case Str.codePointAt (col - 2) l of
      Just x | isPunc x -> trimCol (col - 1) l
      Just _ -> trimComment col l
      _ -> Nothing

  -- TODO: this breaks if "--" is inside a quoted string.
  -- TODO: Block comments?
  trimComment col l =
    case Str.indexOf (Str.Pattern "--") l of
      Just x | x == 0 -> Nothing
      Just x | x < (col - 1) -> trimCol (x + 1) l
      _ -> Just col

  isPunc = (_ == Str.codePointFromChar ' ') || (_ == Str.codePointFromChar ',')

-- | Trims extraneous whitespace from psc error messages.
trimMessage :: String -> String
trimMessage =
  Str.split (Str.Pattern "\n")
    >>> foldl dedent { lines: [], indent: top }
    >>> _.lines
    >>> foldl collapse []
    >>> Str.joinWith "\n"
    >>> Str.trim

  where
  dedent { lines, indent } l
    | l == "" = { lines: Array.snoc lines l, indent }
    | otherwise =
        let
          indent' = Str.length $ Str.takeWhile (_ == Str.codePointFromChar ' ') l
        in
          if indent' < indent then { lines: Array.snoc lines (Str.drop indent' l), indent: indent' }
          else { lines: Array.snoc lines (Str.drop indent l), indent }

  collapse lines l =
    case Array.last lines of
      Just "" | l == "" -> lines
      _ -> Array.snoc lines l
