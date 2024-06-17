module Spago.Glob
  ( gitignoringGlob
  , glob
  ) where

import Spago.Prelude

import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (any)
import Data.String as String
import Data.String.CodePoints as String.CodePoint
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Node.FS.Sync as SyncFS
import Node.Path as Path
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type MicroMatchOptions = { ignore :: Array String }

foreign import micromatch :: MicroMatchOptions -> Array String -> String -> Boolean

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

foreign import globImpl
  :: String
  -> Array String
  -> Effect (Promise (Array String))

gitignoreToMicromatchPatterns :: String -> String -> { ignore :: Array String, patterns :: Array String }
gitignoreToMicromatchPatterns base =
  String.split (String.Pattern "\n")
    >>> map String.trim
    >>> Array.filter (not <<< or [ String.null, isComment ])
    >>> partitionMap
      ( \line -> do
          let negated = isJust $ String.stripPrefix (String.Pattern "!") line
          let pattern = Path.concat [ base, gitignorePatternToMicromatch line ]
          if negated then Left pattern else Right pattern
      )
    >>> Record.rename (Proxy :: Proxy "left") (Proxy :: Proxy "ignore")
    >>> Record.rename (Proxy :: Proxy "right") (Proxy :: Proxy "patterns")

  where
  isComment = isJust <<< String.stripPrefix (String.Pattern "#")
  dropSuffixSlash str = fromMaybe str $ String.stripSuffix (String.Pattern "/") str
  dropPrefixSlash str = fromMaybe str $ String.stripPrefix (String.Pattern "/") str

  leadingSlash str = String.codePointAt 0 str == Just (String.CodePoint.codePointFromChar '/')
  trailingSlash str = String.codePointAt (String.length str - 1) str == Just (String.CodePoint.codePointFromChar '/')

  gitignorePatternToMicromatch :: String -> String
  gitignorePatternToMicromatch pattern
    | trailingSlash pattern = gitignorePatternToMicromatch $ dropSuffixSlash pattern
    | leadingSlash pattern = dropPrefixSlash pattern <> "/**"
    | otherwise = "**/" <> pattern <> "/**"

fsWalk :: String -> Array String -> Array String -> Aff (Array Entry)
fsWalk cwd ignorePatterns includePatterns = Aff.makeAff \cb -> do
  log $ unsafeCoerce { fn: "fsWalk", cwd, ignorePatterns, includePatterns }
  let includeMatcher = micromatch { ignore: [] } includePatterns -- The Stuff we are globbing for.

  -- Pattern for directories which can be outright ignored.
  -- This will be updated whenver a .gitignore is found.
  ignoreMatcherRef <- Ref.new $ micromatch { ignore: [] } ignorePatterns

  -- If this Ref contains `true` because this Aff has been canceled, then deepFilter will always return false.
  canceled <- Ref.new false
  let
    -- Should `fsWalk` recurse into this directory?
    deepFilter :: Entry -> Effect Boolean
    deepFilter entry = Ref.read canceled >>=
      if _ then
        -- The Aff has been canceled, don't recurse into any further directories!
        pure false
      else do
        matcher <- Ref.read ignoreMatcherRef

        let
          relPath = Path.relative cwd entry.path
          matchedIgnore = matcher relPath

        -- log $ unsafeCoerce { fn: "deepFilter", entry, relPath, matchedIgnore }

        pure $ not matchedIgnore

    -- Should `fsWalk` retain this entry for the result array?
    entryFilter :: Entry -> Effect Boolean
    entryFilter entry = do
      -- log $ unsafeCoerce {entry}
      when (isFile entry.dirent && entry.name == ".gitignore") do -- A .gitignore was encountered
        let gitignorePath = entry.path

        -- directory of this .gitignore relative to the directory being globbed
        let base = Path.relative cwd (Path.dirname gitignorePath)

        try (SyncFS.readTextFile UTF8 gitignorePath) >>= case _ of
          Left _ -> pure unit
          Right gitignore -> do
            let { ignore, patterns } = gitignoreToMicromatchPatterns base gitignore
            let gitignored = (micromatch { ignore } <<< pure) <$> patterns
            let wouldConflictWithSearch m = any m includePatterns

            -- Do not add `.gitignore` patterns that explicitly ignore the files
            -- we're searching for;
            --
            -- ex. if `includePatterns` is [".spago/p/aff-1.0.0/**/*.purs"],
            -- and `gitignored` is ["node_modules", ".spago"],
            -- then add "node_modules" to `ignoreMatcher` but not ".spago"
            for_ (filter (not <<< wouldConflictWithSearch) gitignored) \pat -> do
              -- Instead of composing the matcher functions, we could also keep a growing array of
              -- patterns and regenerate the matcher on every append. I don't know which option is
              -- more performant, but composing functions is more convenient.
              let addMatcher currentMatcher = or [ currentMatcher, pat ]
              void $ Ref.modify addMatcher ignoreMatcherRef

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

glob :: String -> Array String -> Aff (Array String)
glob dir patterns = map (withForwardSlashes <<< Path.relative dir) <$> do
  toAffE $ globImpl dir patterns
