module Spago.Glob (gitignoringGlob) where

import Spago.Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (any, fold)
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Aff as Aff
import Effect.Class.Console as Console
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

foreign import direntToString :: DirEnt -> String

instance Show DirEnt where
  show = direntToString

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
          let
            resolve a = Path.concat [ base, a ]
            pat a = withForwardSlashes $ resolve $ unpackPattern a
          case String.stripPrefix (String.Pattern "!") line of
            Just negated -> Left $ pat negated
            Nothing -> Right $ pat line
      )
    >>> (\{ left, right } -> { ignore: left, include: right })

  where
  isComment = isJust <<< String.stripPrefix (String.Pattern "#")
  leadingSlash = String.stripPrefix (String.Pattern "/")
  trailingSlash = String.stripSuffix (String.Pattern "/")

  unpackPattern :: String -> String
  unpackPattern pattern
    | Just a <- trailingSlash pattern = unpackPattern a
    | Just a <- leadingSlash pattern = a <> "/**"
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
    updateGitignore :: Entry -> Effect Unit
    updateGitignore entry =
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
      lift $ Console.log $ "deepFilter: " <> show entry
      isCanceled <- lift $ Ref.read canceled
      guard $ not isCanceled
      shouldIgnore <- lift $ testGlob <$> Ref.read ignoreGlobRef
      pure $ not $ shouldIgnore $ Path.relative cwd entry.path

    -- Should `fsWalk` retain this entry for the result array?
    entryFilter :: Entry -> Effect Boolean
    entryFilter entry = do
      Console.log $ "entryFilter: " <> show entry
      when (isFile entry.dirent && entry.name == ".gitignore") (updateGitignore entry)
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
