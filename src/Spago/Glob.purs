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
  isComment = isJust <<< String.stripPrefix (String.Pattern "#")
  dropSuffixSlash str = fromMaybe str $ String.stripSuffix (String.Pattern "/") str
  dropPrefixSlash str = fromMaybe str $ String.stripPrefix (String.Pattern "/") str

  leadingSlash str = String.codePointAt 0 str == Just (String.CodePoint.codePointFromChar '/')
  trailingSlash str = String.codePointAt (String.length str - 1) str == Just (String.CodePoint.codePointFromChar '/')

  gitignorePatternToGlobPattern :: String -> String
  gitignorePatternToGlobPattern pattern
    | trailingSlash pattern = gitignorePatternToGlobPattern $ dropSuffixSlash pattern
    | leadingSlash pattern = dropPrefixSlash pattern <> "/**"
    | otherwise = "**/" <> pattern <> "/**"

fsWalk :: String -> Array String -> Array String -> Aff (Array Entry)
fsWalk cwd ignorePatterns includePatterns = Aff.makeAff \cb -> do
  let includeMatcher = testGlob { ignore: [], include: includePatterns }

  -- Pattern for directories which can be outright ignored.
  -- This will be updated whenver a .gitignore is found.
  ignoreMatcherRef :: Ref (String -> Boolean) <- Ref.new (testGlob { ignore: [], include: ignorePatterns })

  -- If this Ref contains `true` because this Aff has been canceled, then deepFilter will always return false.
  canceled <- Ref.new false

  let
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

    -- Should `fsWalk` recurse into this directory?
    deepFilter :: Entry -> Effect Boolean
    deepFilter entry = fromMaybe false <$> runMaybeT do
      isCanceled <- lift $ Ref.read canceled
      guard $ not isCanceled
      shouldIgnore <- lift $ Ref.read ignoreMatcherRef
      pure $ not $ shouldIgnore $ Path.relative cwd entry.path

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

gitignoringGlob :: String -> Array String -> Aff (Array String)
gitignoringGlob dir patterns = map (withForwardSlashes <<< Path.relative dir <<< _.path)
  <$> fsWalk dir [ ".git" ] patterns
