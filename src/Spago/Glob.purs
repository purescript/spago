module Spago.Glob (gitignoringGlob) where

import Spago.Prelude

import Data.Array as Array
import Data.String as String
import Data.Foldable (any)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.FS.Sync as SyncFS
import Node.Path as Path
import Record as Record
import Type.Proxy (Proxy(..))

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

  gitignorePatternToMicromatch :: String -> String
  gitignorePatternToMicromatch pattern
    -- Git matches every pattern that does not include a `/` by basename.
    | not $ String.contains (String.Pattern "/") pattern = "**/" <> pattern <> "/**"
    | otherwise =
        -- Micromatch treats every pattern like git treats those starting with '/'.
        dropPrefixSlash pattern
          -- ".spago/" in a .gitignore is the same as ".spago". Micromatch does interpret them differently.
          # dropSuffixSlash

fsWalk :: String -> Array String -> Array String -> Aff (Array Entry)
fsWalk cwd ignorePatterns includePatterns = Aff.makeAff \cb -> do
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
        pure $ not $ matcher (Path.relative cwd entry.path)

    -- Should `fsWalk` retain this entry for the result array?
    entryFilter :: Entry -> Effect Boolean
    entryFilter entry = do
      when (isFile entry.dirent && entry.name == ".gitignore") do -- A .gitignore was encountered
        let gitignorePath = entry.path

        -- directory of this .gitignore relative to the directory being globbed
        let base = Path.relative cwd (Path.dirname gitignorePath)

        try (SyncFS.readTextFile UTF8 gitignorePath) >>= case _ of
          Left _ -> pure unit
          Right gitignore -> do
            let { ignore, patterns } = gitignoreToMicromatchPatterns base gitignore
            let matcherForThisGitignore = micromatch { ignore } patterns

            -- Does the .gitignore contain a pattern which would ignore the very
            -- thing we are asked to look for?
            -- For example, the .gitignore might have the pattern ".spago" but
            -- `includePatterns` contains a glob like .spago/p/foo-3.1.4/**/*.purs
            let anyIncludePatternWouldBeIgnored = any matcherForThisGitignore includePatterns

            -- In such a case, do not use this .gitignore for pruning the search.
            when (not anyIncludePatternWouldBeIgnored) do
              -- Instead of composing the matcher functions, we could also keep a growing array of
              -- patterns and regenerate the matcher on every append. I don't know which option is
              -- more performant, but composing functions is more convenient.
              let addMatcher currentMatcher = or [ currentMatcher, matcherForThisGitignore ]
              void $ Ref.modify addMatcher ignoreMatcherRef

      ignoreMatcher <- Ref.read ignoreMatcherRef
      let path = Path.relative cwd entry.path
      pure $ includeMatcher path && not (ignoreMatcher path)

    options = { entryFilter, deepFilter }

  fsWalkImpl Left Right cb options cwd

  pure $ Aff.Canceler \_ ->
    void $ liftEffect $ Ref.write true canceled

gitignoringGlob :: String -> Array String -> Aff (Array String)
gitignoringGlob dir patterns =
  map (Path.relative dir <<< _.path)
    <$> fsWalk dir [ ".git" ] patterns
