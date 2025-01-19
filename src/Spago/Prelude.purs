module Spago.Prelude
  ( HexString(..)
  , OnlineStatus(..)
  , SpagoBaseEnv
  , isPrefix
  , mkTemp
  , mkTemp'
  , module Spago.Path
  , module Spago.Core.Prelude
  , parTraverseSpago
  , parallelise
  , parseLenientVersion
  , parseUrl
  , partitionEithers
  , shaToHex
  , typoSuggestions
  , unsafeFromRight
  , unsafeLog
  , unsafeStringify
  , withBackoff'
  ) where

import Spago.Core.Prelude

import Control.Parallel as Parallel
import Data.Array as Array
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Extra (levenshtein)
import Data.Traversable (class Traversable)
import Effect.Aff as Aff
import Effect.Now as Now
import JSON (JSON)
import JSON as JSON
import Node.Buffer as Buffer
import Partial.Unsafe (unsafeCrashWith)
import Registry.Sha256 as Registry.Sha256
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Path (class IsPath, RawFilePath, GlobalPath, LocalPath, RootPath, (</>), withForwardSlashes)
import Spago.Path as Path
import Spago.Paths as Paths
import Unsafe.Coerce (unsafeCoerce)

data OnlineStatus = Offline | Online | OnlineBypassCache

derive instance Eq OnlineStatus

type SpagoBaseEnv a =
  { rootPath :: Path.RootPath
  , logOptions :: LogOptions
  | a
  }

unsafeFromRight :: forall e a. Either e a -> a
unsafeFromRight v = Either.fromRight' (\_ -> unsafeCrashWith $ "Unexpected Left: " <> unsafeStringify v) v

parseUrl :: String -> Either String URL
parseUrl = runFn3 parseUrlImpl Left (Right <<< unsafeCoerce)

type URL = { href :: String }

foreign import parseUrlImpl :: forall r. Fn3 (String -> r) (String -> r) String r

foreign import unsafeLog :: forall a. a -> Effect Unit

parallelise :: forall env a. Array (Spago env a) -> Spago env Unit
parallelise actions = do
  env <- ask
  fibers <- liftAff $ Parallel.parSequence (map (Aff.forkAff <<< runSpago env) actions :: Array _)
  liftAff $ for_ fibers Aff.joinFiber

parTraverseSpago :: forall env a b t. Traversable t => (a -> Spago env b) -> t a -> Spago env (t b)
parTraverseSpago action t = do
  env <- ask
  liftAff $ Parallel.parTraverse (runSpago env <<< action) t

shaToHex :: Sha256 -> Effect HexString
shaToHex s = do
  (buffer :: Buffer.Buffer) <- Buffer.fromString (Registry.Sha256.print s) UTF8
  string <- Buffer.toString Hex buffer
  pure $ HexString string

newtype HexString = HexString String

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either.Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Either.Left err -> { fail: [ err ], success: [] }
  Either.Right res -> { fail: [], success: [ res ] }

-- | Unsafely stringify a value by coercing it to `Json` and stringifying it.
unsafeStringify :: forall a. a -> String
unsafeStringify a = JSON.print (unsafeCoerce a :: JSON)

parseLenientVersion :: String -> Either String Version.Version
parseLenientVersion input = Version.parse do
  -- First we ensure there are no leading or trailing spaces.
  String.trim input
    -- Then we remove a 'v' prefix, if present.
    # maybeIdentity (String.stripPrefix (String.Pattern "v"))
    -- Then we group by where the version digits ought to be...
    # String.split (String.Pattern ".")
    -- ...so that we can trim any leading zeros
    # map (maybeIdentity dropLeadingZeros)
    -- and rejoin the string.
    # String.joinWith "."
  where
  maybeIdentity k x = Maybe.fromMaybe x (k x)
  dropLeadingZeros = map (Int.toStringAs Int.decimal) <<< Int.fromString

-- | Attempt an effectful computation with exponential backoff.
withBackoff' :: forall a. Aff a -> Aff (Maybe.Maybe a)
withBackoff' action = withBackoff
  { delay: Aff.Milliseconds 2_000.0
  , action
  , shouldCancel: \_ -> pure true
  , shouldRetry: \attempt -> if attempt > 3 then pure Maybe.Nothing else pure (Maybe.Just action)
  }

type Backoff a =
  { delay :: Aff.Milliseconds
  , action :: Aff a
  , shouldCancel :: Int -> Aff Boolean
  , shouldRetry :: Int -> Aff (Maybe.Maybe (Aff a))
  }

-- | Attempt an effectful computation with exponential backoff, starting with
-- | the provided timeout.
withBackoff :: forall a. Backoff a -> Aff (Maybe.Maybe a)
withBackoff { delay: Aff.Milliseconds timeout, action, shouldCancel, shouldRetry } = do
  let
    runAction attempt action' ms =
      Parallel.sequential $ Foldable.oneOf
        [ Parallel.parallel (map Maybe.Just action')
        , Parallel.parallel (runTimeout attempt ms)
        ]

    runTimeout attempt ms = do
      _ <- Aff.delay (Aff.Milliseconds (Int.toNumber ms))
      shouldCancel attempt >>= if _ then pure Maybe.Nothing else runTimeout attempt (ms * 2)

    loop :: Int -> Maybe.Maybe a -> Aff (Maybe.Maybe a)
    loop attempt = case _ of
      Maybe.Nothing -> do
        maybeRetry <- shouldRetry attempt
        case maybeRetry of
          Maybe.Nothing -> pure Maybe.Nothing
          Maybe.Just newAction -> do
            let newTimeout = Int.floor timeout `Int.pow` (attempt + 1)
            maybeResult <- runAction attempt newAction newTimeout
            loop (attempt + 1) maybeResult
      Maybe.Just result ->
        pure (Maybe.Just result)

  maybeResult <- runAction 0 action (Int.floor timeout)
  loop 1 maybeResult

mkTemp' :: forall m. MonadAff m => Maybe String -> m Path.GlobalPath
mkTemp' maybeSuffix = liftAff do
  -- Get a random string
  (HexString random) <- liftEffect do
    now <- Now.now
    sha <- Sha256.hashString $ show now <> fromMaybe "" maybeSuffix
    shaToHex sha
  -- Return the dir, but don't make it - that's the responsibility of the client
  let tempDirPath = Paths.paths.temp </> String.drop 50 random
  pure tempDirPath

mkTemp :: forall m. MonadAff m => m Path.GlobalPath
mkTemp = mkTemp' Nothing

isPrefix :: String.Pattern -> String -> Boolean
isPrefix p = isJust <<< String.stripPrefix p

typoSuggestions :: ∀ f a. Foldable.Foldable f => (a -> String) -> a -> f a -> Array a
typoSuggestions toString typo allOptions =
  allOptions
    # foldl pickClosestOnes []
    # Array.sortWith snd
    # Array.take 5
    <#> fst
  where
  pickClosestOnes acc item =
    let
      distance = levenshtein (toString typo) (toString item)
    in
      if distance <= 2 then
        (item /\ distance) `Array.cons` acc
      else
        acc
