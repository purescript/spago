module Spago.Glob (gitignoringGlob) where

import Spago.Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (any, fold)
import Data.String as String
import Data.String.CodePoints as String.CodePoint
import Data.Traversable (traverse_)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.FS.Sync as SyncFS
import Node.Path as Path
import Record as Record
import Type.Proxy (Proxy(..))

type MicroMatchOptions = { ignore :: Array String, include :: Array String }

foreign import micromatch :: MicroMatchOptions -> String -> Boolean

splitMicromatch :: MicroMatchOptions -> Array MicroMatchOptions
splitMicromatch { ignore, include } = (\a -> { ignore, include: [ a ] }) <$> include

type Entry = { name :: String, path :: String, dirent :: DirEnt }
type FsWalkOptions = { entryFilter :: Entry -> Effect Boolean, deepFilter :: Entry -> Effect Boolean }

foreign import data DirEnt :: Type
foreign import isFile :: DirEnt -> Boolean
foreign import fsWalkImpl
  :: (forall a b. a -> Either a b)
  -> (forall a b. b -> Either a b)
  -> (Either Error (Array Entry) -> Effect Unit)
  -> FsWalkOptions
  -> String
  -> Effect Unit

gitignoreToMicromatchPatterns :: String -> String -> { ignore :: Array String, include :: Array String }
gitignoreToMicromatchPatterns base =
  String.split (String.Pattern "\n")
    >>> map String.trim
    >>> Array.filter (not <<< or [ String.null, isComment ])
    >>> partitionMap
      ( \line -> do
          let pattern a = Path.concat [ base, gitignorePatternToMicromatch a ]
          case String.stripPrefix (String.Pattern "!") line of
            Just negated -> Left $ pattern negated
            Nothing -> Right $ pattern line
      )
    >>> Record.rename (Proxy :: Proxy "left") (Proxy :: Proxy "ignore")
    >>> Record.rename (Proxy :: Proxy "right") (Proxy :: Proxy "include")

  where
  isComment = isJust <<< String.stripPrefix (String.Pattern "#")
  dropSuffixSlash str = fromMaybe str $ String.stripSuffix (String.Pattern "/") str
  dropPrefixSlash str = fromMaybe str $ String.stripPrefix (String.Pattern "/") str

  leadingSlash str = String.codePointAt 0 str == Just (String.CodePoint.codePointFromChar '/')

  gitignorePatternToMicromatch :: String -> String
  gitignorePatternToMicromatch pattern
    | leadingSlash pattern = dropPrefixSlash pattern <> "/**"
    | otherwise = "**/" <> dropSuffixSlash pattern <> "/**"

fsWalk :: String -> Array String -> Array String -> Aff (Array Entry)
fsWalk cwd ignorePatterns includePatterns = Aff.makeAff \cb -> do
  let includeMatcher = micromatch { ignore: [], include: includePatterns }

  -- Pattern for directories which can be outright ignored.
  -- This will be updated whenver a .gitignore is found.
  ignoreMatcherRef <- Ref.new { ignore: [], include: ignorePatterns }

  -- If this Ref contains `true` because this Aff has been canceled, then deepFilter will always return false.
  canceled <- Ref.new false
  let
    entryGitignore :: Entry -> Effect Unit
    entryGitignore entry =
      try (SyncFS.readTextFile UTF8 entry.path)
        >>= traverse_ \gitignore ->
          let
            base = Path.relative cwd $ Path.dirname entry.path
            gitignored = splitMicromatch $ gitignoreToMicromatchPatterns base gitignore
            canIgnore = not <<< flip any includePatterns
            newIgnores = filter (canIgnore <<< micromatch) gitignored
          in
            void
              $ Ref.modify (_ <> fold newIgnores)
              $ ignoreMatcherRef

    -- Should `fsWalk` recurse into this directory?
    deepFilter :: Entry -> Effect Boolean
    deepFilter entry = fromMaybe false <$> runMaybeT do
      isCanceled <- lift $ Ref.read canceled
      guard $ not isCanceled
      shouldIgnore <- lift $ micromatch <$> Ref.read ignoreMatcherRef
      pure $ not $ shouldIgnore $ Path.relative cwd entry.path

    -- Should `fsWalk` retain this entry for the result array?
    entryFilter :: Entry -> Effect Boolean
    entryFilter entry = do
      when (isFile entry.dirent && entry.name == ".gitignore") (entryGitignore entry)
      ignorePat <- Ref.read ignoreMatcherRef
      let
        ignoreMatcher = micromatch ignorePat
        path = withForwardSlashes $ Path.relative cwd entry.path
      pure $ includeMatcher path && not (ignoreMatcher path)

    options = { entryFilter, deepFilter }

  fsWalkImpl Left Right cb options cwd

  pure $ Aff.Canceler \_ ->
    void $ liftEffect $ Ref.write true canceled

gitignoringGlob :: String -> Array String -> Aff (Array String)
gitignoringGlob dir patterns = map (withForwardSlashes <<< Path.relative dir <<< _.path)
  <$> fsWalk dir [ ".git" ] patterns
