module Options.Applicative.Common (

  liftOpt,
  showOption,


  module Reexport,

  -- * Running parsers
  runParserInfo,
  runParserFully,
  runParser,
  evalParser,

  -- * Low-level utilities
  mapParser,
  treeMapParser,
  optionNames
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Free as Free
import Control.Monad.State.Trans (StateT(..), get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array as Array
import Data.Exists (mkExists, runExists)
import Data.Foldable (any, elem, oneOf)
import Data.List as List
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (un)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Options.Applicative.Internal (class MonadP, NondetT, cut, disamb, enterContext, errorP, exitContext, exitP, getPrefs, hoistMaybe, missingArgP, runReadM, withReadM, (<!>))
import Options.Applicative.Internal.Utils (startsWith)
import Options.Applicative.Types (ArgPolicy(..), Args, Backtracking(..), CReader(..), IsCmdStart(..), MultPE(..), OptHelpInfo(..), OptName(..), OptReader(..), OptTree(..), OptVisibility(..), Option(..), ParseError(..), Parser(..), ParserInfo(..), ParserPrefs(..), SomeParser(..), optVisibility)
import Options.Applicative.Types (Parser, ParserInfo(..), ParserPrefs(..)) as Reexport

showOption :: OptName -> String
showOption (OptLong n) = "--" <> n
showOption (OptShort n) = fromCharArray ['-', n]

optionNames :: forall a. OptReader a -> Array OptName
optionNames (OptReader names _ _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

isOptionPrefix :: OptName -> OptName -> Boolean
isOptionPrefix (OptShort x) (OptShort y) = x == y
isOptionPrefix (OptLong x) (OptLong y) = String.Pattern x `startsWith` y
isOptionPrefix _ _ = false

-- | Create a parser composed of a single option.
liftOpt :: forall a. Option a -> Parser a
liftOpt = OptP

optMatches :: forall m a. MonadP m => Boolean -> OptReader a -> OptWord -> Maybe (StateT Args m a)
optMatches disambiguate opt (OptWord arg1 val) = case opt of
  OptReader names rdr no_arg_err -> do
    guard $ has_name arg1 names
    Just $ do
      args <- get
      let missing_arg = missingArgP (no_arg_err $ showOption arg1) (un CReader rdr).crCompleter
      Tuple arg' args' <- case maybe args (_ `List.Cons` args) val of
        List.Nil -> lift missing_arg
        List.Cons arg' args' -> pure $ Tuple arg' args'
      put args'
      lift $ runReadM (withReadM (errorFor arg1) (un CReader rdr).crReader) arg'

  FlagReader names x -> do
    guard $ has_name arg1 names
    -- #242 Flags/switches succeed incorrectly when given an argument.
    -- We'll not match a long option for a flag if there's a word attached.
    -- This was revealing an implementation detail as
    -- `--foo=val` was being parsed as `--foo -val`, which is gibberish.
    guard $ is_short arg1 || isNothing val
    Just $ do
      args <- get
      let val' = (toCharArray >>> \s -> '-' `Array.cons` s) <$> val
      put $ maybe args (fromCharArray >>> flip List.Cons args) val'
      pure x
  _ -> Nothing
  where
    errorFor name msg = "option " <> showOption name <> ": " <> msg

    is_short (OptShort _) = true
    is_short (OptLong _)  = false

    has_name a
      | disambiguate = any (isOptionPrefix a)
      | otherwise = elem a

isArg :: forall a. OptReader a -> Boolean
isArg (ArgReader _) = true
isArg _ = false

data OptWord = OptWord OptName (Maybe String)

parseWord :: String -> Maybe OptWord
parseWord = toCharArray >>> List.fromFoldable >>> go
  where
    go ('-' List.: '-' List.: w) = Just $ let
      Tuple opt arg = case List.span (_ /= '=') w of
        { rest: List.Nil} -> (Tuple w Nothing)
        { init: w', rest: List.Cons _ rest} -> (Tuple w' (Just rest))
      in OptWord (OptLong (fromCharArray $ Array.fromFoldable opt)) (fromCharArray <<< Array.fromFoldable <$> arg)
    go ('-' List.: w) = case w of
      List.Nil -> Nothing
      (a List.: rest) -> Just $ let
        arg = rest <$ guard (not (List.null rest))
        in OptWord (OptShort a) (fromCharArray <<< Array.fromFoldable <$> arg)
    go _ = Nothing

searchParser :: forall m a. Monad m
             => (forall r . Option r -> NondetT m (Parser r))
             -> Parser a -> NondetT m (Parser a)
searchParser _ (NilP _) = empty
searchParser f (OptP opt) = f opt
searchParser f (MultP e) = runExists (\(MultPE p1 p2) ->
  let
    a = searchParser f p1 <#> \p1' -> (p1' <*> p2)
    b = searchParser f p2 <#> \p2' -> (p1 <*> p2')
  in a <!> b
  ) e
searchParser f (AltP p1 p2) = oneOf
  [ searchParser f p1
  , searchParser f p2 ]
searchParser f (BindP e) = e # Free.resume' (\p k ->
  oneOf
    [ searchParser f p <#> \p' -> BindP $ Free.liftF p' >>= k
    , case evalParser p of
        Nothing -> empty
        Just aa -> searchParser f (BindP $ k aa) ]
  ) (const empty)

searchOpt :: forall m a. MonadP m => ParserPrefs -> OptWord -> Parser a
          -> NondetT (StateT Args m) (Parser a)
searchOpt pprefs w = searchParser \opt -> do
  let disambiguate = (un ParserPrefs pprefs).prefDisambiguate && optVisibility opt > Internal
  case optMatches disambiguate (un Option opt).optMain w of
    Just matcher -> lift $ map pure matcher
    Nothing -> empty

searchArg :: forall m a. MonadP m => ParserPrefs -> String -> Parser a
          -> NondetT (StateT Args m) (Parser a)
searchArg prefs arg = searchParser \opt -> do
  when (isArg (un Option opt).optMain) cut
  case (un Option opt).optMain of
    CmdReader _ _ f ->
      case (Tuple (f arg) (un ParserPrefs prefs).prefBacktrack) of
        (Tuple (Just subp) NoBacktrack) -> lift $ do
          args <- get <* put List.Nil
          map pure <<< lift $ enterContext arg subp *> runParserInfo subp args <* exitContext

        (Tuple (Just subp) Backtrack) -> map pure <<< lift <<< StateT $ \args -> do
          enterContext arg subp *> runParser (un ParserInfo subp).infoPolicy CmdStart (un ParserInfo subp).infoParser args <* exitContext

        (Tuple (Just subp) SubparserInline) -> lift $ do
          lift $ enterContext arg subp
          pure $ (un ParserInfo subp).infoParser

        (Tuple (Nothing) _)  -> empty
    ArgReader rdr ->
      map pure <<< lift <<< lift $ runReadM (un CReader rdr).crReader arg
    _ -> empty

stepParser :: forall m a. MonadP m => ParserPrefs -> ArgPolicy -> String
           -> Parser a -> NondetT (StateT Args m) (Parser a)
stepParser pprefs AllPositionals arg p = searchArg pprefs arg p
stepParser pprefs ForwardOptions arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p <|> searchArg pprefs arg p
  Nothing -> searchArg pprefs arg p
stepParser pprefs _ arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p
  Nothing -> searchArg pprefs arg p


-- | Apply a 'Parser' to a command line, and return a result and leftover
-- | arguments.  This function returns an error if any parsing error occurs, or
-- | if any options are missing and don't have a default value.
runParser :: forall m a. MonadP m => ArgPolicy -> IsCmdStart -> Parser a -> Args -> m (Tuple a Args)
runParser policy isCmdStart p args = case args of
  List.Nil -> exitP isCmdStart policy p result
  List.Cons "--" argt | policy /= AllPositionals ->  runParser AllPositionals CmdCont p argt
  List.Cons arg argt -> do
    prefs <- getPrefs
    (Tuple mp' args') <- do_step prefs arg argt
    case mp' of
      Nothing -> hoistMaybe (unexpectedError arg p) result
      Just p' -> runParser (newPolicy arg) CmdCont p' args'
  where
    result = (Tuple) <$> evalParser p <*> pure args
    do_step prefs arg argt = (_ `runStateT` argt)
                           <<< disamb (not (un ParserPrefs prefs).prefDisambiguate)
                           $ stepParser prefs policy arg p

    newPolicy a = case policy of
      NoIntersperse -> if isJust (parseWord a) then NoIntersperse else AllPositionals
      x             -> x

unexpectedError :: forall x. String -> Parser x -> ParseError
unexpectedError arg p = UnexpectedError arg $ SomeParser $ mkExists p

runParserInfo :: forall m a. MonadP m => ParserInfo a -> Args -> m a
runParserInfo i = runParserFully (un ParserInfo i).infoPolicy (un ParserInfo i).infoParser

runParserFully :: forall m a. MonadP m => ArgPolicy -> Parser a -> Args -> m a
runParserFully policy p args = do
  (Tuple r args') <- runParser policy CmdStart p args
  case args' of
    List.Nil -> pure r
    List.Cons head _ -> errorP $ unexpectedError head (pure unit)

-- | The default value of a 'Parser'.  This function returns an error if any of
-- | the options don't have a default value.
evalParser :: forall a. Parser a -> Maybe a
evalParser (NilP r) = Just r
evalParser (OptP _) = Nothing
evalParser (MultP e) = runExists (\(MultPE p1 p2) -> evalParser p1 <*> evalParser p2) e
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2
evalParser (BindP e) = e # Free.resume' (\p k ->
    evalParser p >>= evalParser <<< BindP <<< k
  ) Just

-- | Map a polymorphic function over all the options of a parser, and collect
-- | the results in a list.
mapParser :: forall a b. (forall x. OptHelpInfo -> Option x -> b)
          -> Parser a -> Array b
mapParser f = flatten <<< treeMapParser f
  where
    flatten (Leaf x) = [x]
    flatten (MultNode xs) = xs >>= flatten
    flatten (AltNode xs) = xs >>= flatten

-- | Like 'mapParser', but collect the results in a tree structure.
treeMapParser :: forall a b. (forall x. OptHelpInfo -> Option x -> b)
          -> Parser a
          -> OptTree b
treeMapParser g = simplify <<< go false false false g
  where
    has_default :: forall a'. Parser a' -> Boolean
    has_default p = isJust (evalParser p)

    go :: forall a' b'. Boolean -> Boolean -> Boolean
       -> (forall x. OptHelpInfo -> Option x -> b')
       -> Parser a'
       -> OptTree b'
    go _ _ _ _ (NilP _) = MultNode []
    go m d r f (OptP opt)
      | optVisibility opt > Internal
      = Leaf (f (OptHelpInfo {hinfoMulti: m, hinfoDefault: d, hinfoUnreachableArgs: r}) opt)
      | otherwise
      = MultNode []
    go m d r f (MultP e) = runExists (\(MultPE p1 p2) ->
        let r' = r || hasArg p1 in MultNode [go m d r f p1, go m d r' f p2]
      )
      e

    go m d r f (AltP p1 p2) = AltNode [go m d' r f p1, go m d' r f p2]
      where d' = d || has_default p1 || has_default p2
    go _ d r f (BindP e) = e # Free.resume' (\p k ->
      let go' = go true d r f p
        in case evalParser p of
          Nothing -> go'
          Just aa -> MultNode [ go', go true d r f (BindP $ k aa) ]
    ) (const $ MultNode [])

    hasArg :: forall r. Parser r -> Boolean
    hasArg (NilP _) = false
    hasArg (OptP p) = isArg (un Option p).optMain
    hasArg (MultP e) = runExists (\(MultPE p1 p2) -> hasArg p1 || hasArg p2) e
    hasArg (AltP p1 p2) = hasArg p1 || hasArg p2
    hasArg (BindP e) = e # Free.resume' (\p _ -> hasArg p) (const false)


simplify :: forall a. OptTree a -> OptTree a
simplify (Leaf x) = Leaf x
simplify (MultNode xs) =
  case xs >>= (simplify >>> remove_mult) of
    [x] -> x
    xs' -> MultNode xs'
  where
    remove_mult (MultNode ts) = ts
    remove_mult t = [t]
simplify (AltNode xs) =
  case xs >>= (simplify >>> remove_alt) of
    []  -> MultNode []
    [x] -> x
    xs' -> AltNode xs'
  where
    remove_alt (AltNode ts) = ts
    remove_alt (MultNode []) = []
    remove_alt t = [t]
