module Spago.Glob (gitignoringGlob) where

import Spago.Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (any, fold)
import Data.String as String
import Data.String as String.CodePoint
import Data.Traversable (traverse_)
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
  let firstIgnoreGlob = { ignore: [], include: ignorePatterns }
  ignoreGlobRef :: Ref Glob <- Ref.new firstIgnoreGlob
  -- We recompute the ignoreMatcher every time we update the ignoreMatcherRef
  ignoreMatcherRef :: Ref (String -> Boolean) <- Ref.new (testGlob firstIgnoreGlob)

  -- If this Ref contains `true` because this Aff has been canceled, then deepFilter will always return false.
  canceled <- Ref.new false

  let
    -- Update the ignoreMatcherRef with the patterns from a .gitignore file
    updateIgnoreMatcherWithGitignore :: Entry -> Effect Unit
    updateIgnoreMatcherWithGitignore entry = do
      try (SyncFS.readTextFile UTF8 entry.path)
        >>= traverse_ \gitignore -> do
          let
            -- directory of this .gitignore relative to the directory being globbed
            base = Path.relative cwd $ Path.dirname entry.path
            glob = gitignoreFileToGlob base gitignore
            pats = splitGlob glob
            patOk g = not $ any (testGlob g) includePatterns
            newPats = filter patOk pats
          currentGlob <- Ref.read ignoreGlobRef
          let newGlob = currentGlob <> fold newPats
          void $ Ref.write newGlob ignoreGlobRef
          void $ Ref.write (testGlob newGlob) ignoreMatcherRef

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
