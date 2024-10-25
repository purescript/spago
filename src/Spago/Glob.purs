-- | We used to just FFI to `fast-glob` for all globbing business, but that was
-- | way too slow. See #1182 for more info and a starting point.
-- | All of this code (and the FFI file) is a series of attempts to make globbing
-- | reasonably performant while still supporting all of our usecases, like ignoring
-- | files based on `.gitignore` files.
module Spago.Glob (gitignoringGlob) where

import Spago.Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (any, traverse_)
import Data.String as String
import Data.String as String.CodePoint
import Effect.Aff as Aff
import Effect.Ref as Ref
import Spago.FS as FS
import Spago.Path as Path

type Glob =
  { ignore :: Array String
  , include :: Array String
  }

foreign import testGlob :: Glob -> AdHocFilePath -> Boolean

splitGlob :: Glob -> Array Glob
splitGlob { ignore, include } = (\a -> { ignore, include: [ a ] }) <$> include

type Entry = { name :: String, path :: GlobalPath, dirent :: DirEnt }
type FsWalkOptions = { entryFilter :: Entry -> Effect Boolean, deepFilter :: Entry -> Effect Boolean }

-- https://nodejs.org/api/fs.html#class-fsdirent
foreign import data DirEnt :: Type
foreign import isFile :: DirEnt -> Boolean

-- https://www.npmjs.com/package/picomatch#scan
foreign import scanPattern :: String -> PatternInfo
type PatternInfo =
  { prefix :: String
  , input :: String
  , start :: Int
  , base :: String
  , glob :: String
  , isBrace :: Boolean
  , isBracket :: Boolean
  , isGlob :: Boolean
  , isExtglob :: Boolean
  , isGlobstar :: Boolean
  , negated :: Boolean
  }

foreign import fsWalkImpl
  :: (forall a b. a -> Either a b)
  -> (forall a b. b -> Either a b)
  -> (Either Error (Array Entry) -> Effect Unit)
  -> FsWalkOptions
  -> RootPath
  -> Effect Unit

gitignoreFileToGlob :: LocalPath -> String -> Glob
gitignoreFileToGlob root =
  String.split (String.Pattern "\n")
    >>> map String.trim
    >>> Array.filter (not String.null && not isComment)
    >>> partitionMap
      ( \line -> do
          let pattern lin = Path.localPart $ withForwardSlashes $ root </> gitignorePatternToGlobPattern lin
          case String.stripPrefix (String.Pattern "!") line of
            Just negated -> Left $ pattern negated
            Nothing -> Right $ pattern line
      )
    >>> (\{ left, right } -> { ignore: left, include: right })

  where
  isComment = isPrefix (String.Pattern "#")
  dropSuffixSlash str = fromMaybe str $ String.stripSuffix (String.Pattern "/") str
  dropPrefixSlash str = fromMaybe str $ String.stripPrefix (String.Pattern "/") str

  leadingSlash str = String.codePointAt 0 str == Just (String.CodePoint.codePointFromChar '/')
  trailingSlash str = String.codePointAt (String.length str - 1) str == Just (String.CodePoint.codePointFromChar '/')

  gitignorePatternToGlobPattern :: String -> String
  gitignorePatternToGlobPattern pattern
    | trailingSlash pattern = gitignorePatternToGlobPattern $ dropSuffixSlash pattern
    | leadingSlash pattern = dropPrefixSlash pattern <> "/**"
    | otherwise = "**/" <> pattern <> "/**"

fsWalk :: RootPath -> Array String -> Array String -> Aff (Array Entry)
fsWalk root ignorePatterns includePatterns = Aff.makeAff \cb -> do
  let includeMatcher = testGlob { ignore: [], include: includePatterns }

  -- Pattern for directories which can be outright ignored.
  -- This will be updated whenver a .gitignore is found.
  ignoreMatcherRef :: Ref (AdHocFilePath -> Boolean) <- Ref.new (testGlob { ignore: [], include: ignorePatterns })

  -- If this Ref contains `true` because this Aff has been canceled, then deepFilter will always return false.
  canceled <- Ref.new false

  let
    -- Update the ignoreMatcherRef with the patterns from a .gitignore file
    updateIgnoreMatcherWithGitignore :: Entry -> Effect Unit
    updateIgnoreMatcherWithGitignore entry = do
      let
        -- directory of this .gitignore relative to the directory being globbed
        base = Path.dirname entry.path `Path.relativeTo` root

      try (FS.readTextFileSync entry.path) >>= traverse_ \gitignore -> do
        let
          gitignored = testGlob <$> (splitGlob $ gitignoreFileToGlob base gitignore)

          -- Do not add `.gitignore` patterns that explicitly ignore the files
          -- we're searching for;
          --
          -- ex. if `includePatterns` is [".spago/p/aff-1.0.0/**/*.purs"],
          -- and `gitignored` is ["node_modules", ".spago"],
          -- then add "node_modules" to `ignoreMatcher` but not ".spago"
          wouldConflictWithSearch matcher = any matcher includePatterns

          newMatchers = or $ filter (not <<< wouldConflictWithSearch) gitignored

          -- Another possible approach could be to keep a growing array of patterns and
          -- regenerate the matcher on every gitignore. We have tried that (see #1234),
          -- and turned out to be 2x slower. (see #1242, and #1244)
          -- Composing functions is faster, but there's the risk of blowing the stack
          -- (see #1231) - when this was introduced in #1210, every match from the
          -- gitignore file would be `or`ed to the previous matcher, which would create
          -- a very long (linear) call chain - in this latest iteration we are `or`ing the
          -- new matchers together, then the whole thing with the previous matcher.
          -- This is still prone to stack issues, but we now have a tree so it should
          -- not be as dramatic.
          addMatcher currentMatcher = or [ currentMatcher, newMatchers ]

        Ref.modify_ addMatcher ignoreMatcherRef

    -- The base of every includePattern
    -- The base of a pattern is its longest non-glob prefix.
    -- For example: foo/bar/*/*.purs => foo/bar
    --              **/spago.yaml => ""
    includePatternBases :: Array String
    includePatternBases = map (_.base <<< scanPattern) includePatterns

    matchesAnyPatternBase :: String -> Boolean
    matchesAnyPatternBase relDirPath = any matchesPatternBase includePatternBases
      where
      matchesPatternBase :: String -> Boolean
      matchesPatternBase "" =
        -- Patterns which have no base, for example **/spago.yaml, match every directory.
        true
      matchesPatternBase patternBase | String.length relDirPath < String.length patternBase =
        -- The directoryPath is shorter than the patterns base, so in order for this pattern to
        -- match anything in this directory, the directories path must be a prefix of the patterns base.
        -- For example: pattern     = .spago/p/unfoldable-6.0.0/src/**/*.purs
        --              patternBase = .spago/p/unfoldable-6.0.0/src
        --              relDirPath  = .spago/p/
        -- => relDirPath is a prefix of patternBase => the directory matches
        --
        -- Or in the negative case:
        --              pattern     = .spago/p/unfoldable-6.0.0/src/**/*.purs
        --              patternBase = .spago/p/unfoldable-6.0.0/src
        --              relDirPath  = .spago/p/arrays-7.3.0
        -- => relDirPath is not a prefix of patternBase => the directory does not match
        String.Pattern relDirPath `isPrefix` patternBase
      matchesPatternBase patternBase | otherwise =
        -- The directoryPath is longer than the patterns base, so the directoryPath is more specific.
        -- In order for this pattern to match anything in this directory, the patterns base must be a
        -- prefix of the directories path.
        -- For example: pattern     = .spago/p/unfoldable-6.0.0/src/**/*.purs
        --              patternBase = .spago/p/unfoldable-6.0.0/src
        --              relDirPath  = .spago/p/unfoldable-6.0.0/src/Data
        -- => patternBase is a prefix of relDirPath => the directory matches
        String.Pattern patternBase `isPrefix` relDirPath

    relPath :: Entry -> String
    relPath entry = Path.localPart $ withForwardSlashes entry.path `Path.relativeTo` root

    -- Should `fsWalk` recurse into this directory?
    deepFilter :: Entry -> Effect Boolean
    deepFilter entry = fromMaybe false <$> runMaybeT do
      isCanceled <- lift $ Ref.read canceled
      guard $ not isCanceled
      shouldIgnore <- lift $ Ref.read ignoreMatcherRef
      let path = relPath entry
      guard $ not $ shouldIgnore path

      -- Only if the path of this directory matches any of the patterns base path,
      -- can anything in this directory possibly match the corresponding full pattern.
      pure $ matchesAnyPatternBase path

    -- Should `fsWalk` retain this entry for the result array?
    entryFilter :: Entry -> Effect Boolean
    entryFilter entry = do
      when (isFile entry.dirent && entry.name == ".gitignore") do
        updateIgnoreMatcherWithGitignore entry
      ignoreMatcher <- Ref.read ignoreMatcherRef
      let path = relPath entry
      pure $ includeMatcher path && not (ignoreMatcher path)

    options = { entryFilter, deepFilter }

  fsWalkImpl Left Right cb options root

  pure $ Aff.Canceler \_ ->
    void $ liftEffect $ Ref.write true canceled

gitignoringGlob :: RootPath -> Array String -> Aff (Array LocalPath)
gitignoringGlob root patterns = do
  entries <- fsWalk root [ ".git" ] patterns
  pure $ entries <#> \e -> e.path `Path.relativeTo` root
