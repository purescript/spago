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
import Data.Foldable (all, any, traverse_)
import Data.String as String
import Data.String as String.CodePoint
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.FS.Sync as SyncFS
import Node.Path as Path

type Glob =
  { ignore :: Array String
  , include :: Array String
  }

foreign import testGlob :: Glob -> String -> Boolean

splitGlob :: Glob -> Array Glob
splitGlob { ignore, include } = (\a -> { ignore, include: [ a ] }) <$> include

type Entry = { name :: String, path :: String, dirent :: DirEnt }
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
  -> String
  -> Effect Unit

gitignoreFileToGlob :: FilePath -> String -> Glob
gitignoreFileToGlob base =
  String.split (String.Pattern "\n")
    >>> map String.trim
    >>> Array.filter (not <<< or [ String.null, isComment ])
    >>> partitionMap
      ( \line -> do
          let pattern lin = withForwardSlashes $ Path.concat [ base, gitignorePatternToGlobPattern lin ]
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

fsWalk :: GlobParams -> Aff (Array Entry)
fsWalk { cwd, ignorePatterns, includePatterns } = Aff.makeAff \cb -> do
  let includeMatcher = testGlob { ignore: [], include: includePatterns }

  -- Pattern for directories which can be outright ignored.
  -- This will be updated whenver a .gitignore is found.
  ignoreMatcherRef :: Ref (String -> Boolean) <- Ref.new (testGlob { ignore: [], include: ignorePatterns })

  -- If this Ref contains `true` because this Aff has been canceled, then deepFilter will always return false.
  canceled <- Ref.new false

  let
    -- The base of every includePattern
    -- The base of a pattern is its longest non-glob prefix.
    -- For example: foo/bar/*/*.purs => foo/bar
    --              **/spago.yaml => ""
    includePatternBases :: Array String
    includePatternBases = map (_.base <<< scanPattern) includePatterns

    allIncludePatternsHaveBase = all (not <<< String.null) includePatternBases

    -- Update the ignoreMatcherRef with the patterns from a .gitignore file
    updateIgnoreMatcherWithGitignore :: Entry -> Effect Unit
    updateIgnoreMatcherWithGitignore entry = do
      let
        gitignorePath = entry.path
        -- directory of this .gitignore relative to the directory being globbed
        base = Path.relative cwd (Path.dirname gitignorePath)

      try (SyncFS.readTextFile UTF8 entry.path) >>= traverse_ \gitignore -> do
        let
          gitignored = testGlob <$> (splitGlob $ gitignoreFileToGlob base gitignore)

          -- Do not add `.gitignore` patterns that explicitly ignore the files
          -- we're searching for;
          --
          -- ex. if `includePatterns` is [".spago/p/aff-1.0.0/**/*.purs"],
          -- and `gitignored` is ["node_modules", ".spago"],
          -- then add "node_modules" to `ignoreMatcher` but not ".spago"
          wouldConflictWithSearch matcher = any matcher includePatternBases

          newMatchers :: Array (String -> Boolean)
          newMatchers | allIncludePatternsHaveBase = filter (not <<< wouldConflictWithSearch) gitignored
          newMatchers = do
            -- Some of the include patterns don't have a base,
            -- e.g. there is an include pattern like "*/foo/bar" or "**/.spago".
            -- In this case, do not attempt to determine whether the gitignore
            -- file would exclude some of the target paths. Instead always respect
            -- the .gitignore.
            gitignored

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
          addMatcher currentMatcher = or $ Array.cons currentMatcher newMatchers

        Ref.modify_ addMatcher ignoreMatcherRef

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

    -- Should `fsWalk` recurse into this directory?
    deepFilter :: Entry -> Effect Boolean
    deepFilter entry = fromMaybe false <$> runMaybeT do
      isCanceled <- lift $ Ref.read canceled
      guard $ not isCanceled
      let relPath = withForwardSlashes $ Path.relative cwd entry.path
      shouldIgnore <- lift $ Ref.read ignoreMatcherRef
      guard $ not $ shouldIgnore relPath

      -- Only if the path of this directory matches any of the patterns base path,
      -- can anything in this directory possibly match the corresponding full pattern.
      pure $ matchesAnyPatternBase relPath

    -- Should `fsWalk` retain this entry for the result array?
    entryFilter :: Entry -> Effect Boolean
    entryFilter entry = do
      when (isFile entry.dirent && entry.name == ".gitignore") do
        updateIgnoreMatcherWithGitignore entry
      ignoreMatcher <- Ref.read ignoreMatcherRef
      let path = withForwardSlashes $ Path.relative cwd entry.path
      pure $ includeMatcher path && not (ignoreMatcher path)

    options = { entryFilter, deepFilter }

  fsWalkImpl Left Right cb options cwd

  pure $ Aff.Canceler \_ ->
    void $ liftEffect $ Ref.write true canceled

type GlobParams = { ignorePatterns :: Array String, includePatterns :: Array String, cwd :: FilePath }

gitignoringGlob :: GlobParams -> Aff (Array String)
gitignoringGlob { cwd, ignorePatterns, includePatterns } = map (withForwardSlashes <<< Path.relative cwd <<< _.path)
  <$> fsWalk
    { cwd
    , ignorePatterns: ignorePatterns
        -- The ones in the base directory are always ignored
        <> [ ".git", "spago.yaml" ]
    , includePatterns
    }
