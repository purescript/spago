  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
module Options.Applicative.Extra
  ( helper
  , hsubparser
  , execParser
  -- , execParserMaybe
  , customExecParser
  -- , customExecParserMaybe
  , execParserPure
  , getParseResult
  , handleParseResult
  , parserFailure
  , renderFailure
  -- , ParserFailure(..)
  -- , overFailure
  -- , ParserResult(..)
  -- , ParserPrefs(..)
  -- , CompletionResult(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fold)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Exists (runExists)
import Data.Function (on)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over, un)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import ExitCodes (ExitCode)
import ExitCodes as ExitCode
import Node.Encoding (Encoding(..))
import Node.Process (argv, exit, stderr, stdout)
import Node.Stream as Stream
import Options.Applicative.BashCompletion (bashCompletionParser)
import Options.Applicative.Builder (abortOption, defaultPrefs, help, hidden, long, metavar, short)
import Options.Applicative.Builder.Internal (CommandFields, Mod(..), mkCommand, mkParser)
import Options.Applicative.Common (mapParser, runParserInfo, showOption)
import Options.Applicative.Help (editDistance, errorHelp, footerHelp, headerHelp, indent, missingDesc, parserHelp, parserUsage, renderHelp, stringChunk, suggestionsHelp, usageHelp, vcatChunks, (.$.), (<<+>>))
import Options.Applicative.Internal (contextNames, runP)
import Options.Applicative.Internal.Utils (startsWith, unWords, (<**>))
import Options.Applicative.Types (CompletionResult(..), Context(..), IsCmdStart(..), OptHelpInfo(..), OptReader(..), Option(..), ParseError(..), Parser, ParserFailure(..), ParserHelp, ParserInfo(..), ParserPrefs(..), ParserResult(..), SomeParser(..))


getArgs :: Effect (Array String)
getArgs = argv <#> Array.drop 2

getProgName :: Effect String
getProgName = argv <#> \args -> fromMaybe "" do
  executablePath <- Array.index args 1
  Array.last $ String.split (String.Pattern "/") executablePath

exitSuccess :: forall t270. Effect t270
exitSuccess = exit $ fromEnum ExitCode.Success

exitWith :: forall void. ExitCode -> Effect void
exitWith c = exit $ fromEnum c

-- | A hidden "helper" option which always fails. Use this to
-- | add the `--help` flag to your CLI parser
-- |
-- | A common usage pattern is to apply this applicatively when
-- | creating a 'ParserInfo'
-- |
-- | > opts :: ParserInfo Sample
-- | > opts = info (sample <**> helper) mempty
helper :: forall a. Parser (a -> a)
helper = abortOption ShowHelpText $ fold
  [ long "help"
  , short 'h'
  , help "Show this help text"
  , hidden ]

-- | Builder for a command parser with a "helper" option attached.
-- | Used in the same way as `subparser`, but includes a `--help|-h` inside
-- | the subcommand.
hsubparser :: forall a. Mod CommandFields a -> Parser a
hsubparser m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" `append` m
    groupName /\ cmds /\ subs /\ unit = mkCommand m
    rdr = CmdReader groupName cmds (map add_helper <<< subs)
    add_helper = over ParserInfo \pinfo -> pinfo
      { infoParser = pinfo.infoParser <**> helper}

-- | Run a program description.
-- |
-- | Parse command line arguments. Display help text and exit if any parse error
-- | occurs.
execParser :: forall a. ParserInfo a -> Effect a
execParser = customExecParser defaultPrefs

-- | Run a program description with custom preferences.
customExecParser :: forall a. ParserPrefs -> ParserInfo a -> Effect a
customExecParser pprefs pinfo
  = execParserPure pprefs pinfo <$> getArgs
  >>= handleParseResult

-- | Handle `ParserResult`.
handleParseResult :: forall a. ParserResult a -> Effect a
handleParseResult (Success a) = pure a
handleParseResult (Failure failure) = do
      progn <- getProgName
      let
        Tuple msg exit = renderFailure failure progn
        stream = case exit of
          ExitCode.Success -> stdout
          _           -> stderr
      void $ Stream.writeString stream UTF8 (msg <> "\n") mempty
      exitWith exit
handleParseResult (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- (un CompletionResult compl).execCompletion progn
      void $ Stream.writeString stdout UTF8 msg mempty
      exitSuccess

-- | Extract the actual result from a `ParserResult` value.
-- |
-- | This function returns 'Nothing' in case of errors.  Possible error messages
-- | or completion actions are simply discarded.
-- |
-- | If you want to display error messages and invoke completion actions
-- | appropriately, use 'handleParseResult' instead.
getParseResult :: forall a. ParserResult a -> Maybe a
getParseResult (Success a) = Just a
getParseResult _ = Nothing

-- | The most general way to run a program description in pure code.
execParserPure :: forall a. ParserPrefs       -- ^ Global preferences for this parser
               -> ParserInfo a      -- ^ Description of the program to run
               -> Array String          -- ^ Program arguments
               -> ParserResult a
execParserPure pprefs pinfo args =
  case runP p pprefs of
    Tuple (Right (Right r)) _ -> Success r
    Tuple (Right (Left c)) _ -> CompletionInvoked c
    Tuple (Left err) ctx -> Failure $ parserFailure pprefs pinfo err ctx
  where
    pinfo' = pinfo # over ParserInfo \i -> i
      { infoParser = (Left <$> bashCompletionParser pinfo pprefs)
                 <|> (Right <$> i.infoParser) }
    p = runParserInfo pinfo' $ List.fromFoldable $ args

-- | Generate a `ParserFailure` from a `ParseError` in a given `Context`.
-- |
-- | This function can be used, for example, to show the help text for a parser:
-- |
-- | `handleParseResult <<< Failure $ parserFailure pprefs pinfo ShowHelpText mempty`
parserFailure :: forall a. ParserPrefs -> ParserInfo a
              -> ParseError -> Array Context
              -> ParserFailure ParserHelp
parserFailure pprefs pinfo msg ctx = ParserFailure $ \progn ->
  let h = with_context ctx pinfo \names pinfo' -> fold
            [ base_help pinfo'
            , usage_help progn names pinfo'
            , suggestion_help
            , error_help
            ]
  in h /\ exit_code /\ (un ParserPrefs pprefs).prefColumns /\ unit
  where
    exit_code = case msg of
      ErrorMsg _        -> (un ParserInfo pinfo).infoFailureCode
      MissingError _ _    -> (un ParserInfo pinfo).infoFailureCode
      ExpectsArgError _ -> (un ParserInfo pinfo).infoFailureCode
      UnexpectedError _ _ -> (un ParserInfo pinfo).infoFailureCode
      ShowHelpText       -> ExitCode.Success
      InfoMsg _         -> ExitCode.Success

    with_context :: forall x c. Array Context
                 -> ParserInfo x
                 -> (forall b . Array String -> ParserInfo b -> c)
                 -> c
    with_context arr i f = case Array.head arr of
      Nothing -> f [] i
      Just (Context _ e) -> runExists (\i' -> f (contextNames arr) i') e

    usage_help :: forall x. String -> Array String -> ParserInfo x -> ParserHelp
    usage_help progn names (ParserInfo i) = case msg of
      InfoMsg _
        -> mempty
      _
        -> usageHelp $ vcatChunks
          [ pure $ parserUsage pprefs (i.infoParser) $ unWords $ [progn] <> names
          , map (indent 2) i.infoProgDesc ]

    error_help = errorHelp $ case msg of
      ShowHelpText
        -> mempty

      ErrorMsg m
        -> stringChunk m

      InfoMsg  m
        -> stringChunk m

      MissingError CmdStart _
        | (un ParserPrefs pprefs).prefShowHelpOnEmpty
        -> mempty

      MissingError _ (SomeParser e)
        -> runExists (\x -> stringChunk "Missing:" <<+>> missingDesc pprefs x) e

      ExpectsArgError x
        -> stringChunk $ "The option `" <> x <> "` expects an argument."

      UnexpectedError arg _
        ->
          let
            --
            -- This gives us the same error we have always
            -- reported
            msg' = if startsWith (String.Pattern "-") arg
              then "Invalid option `" <> arg <> "'"
              else "Invalid argument `" <> arg <> "'"
          in stringChunk msg'

    suggestion_help = suggestionsHelp $ case msg of
      UnexpectedError arg (SomeParser x)
        --
        -- We have an unexpected argument and the parser which
        -- it's running over.
        --
        -- We can make a good help suggestion here if we do
        -- a levenstein distance between all possible suggestions
        -- and the supplied option or argument.
        ->
          let
            --
            -- Not using chunked here, as we don't want to
            -- show "Did you mean" if there's nothing there
            -- to show
            suggestions = (.$.) <$> prose
                                <*> (indent 4 <$> (vcatChunks <<< map stringChunk $ good ))

            --
            -- We won't worry about the 0 case, it won't be
            -- shown anyway.
            prose       = if Array.length good < 2
                            then stringChunk "Did you mean this?"
                            else stringChunk "Did you mean one of these?"
            --
            -- Suggestions we will show, they're close enough
            -- to what the user wrote
            good        = Array.filter isClose possibles

            --
            -- Bit of an arbitrary decision here.
            -- Edit distances of 1 or 2 will give hints
            isClose a   = on editDistance toCharArray a arg < 3

            --
            -- Similar to how bash completion works.
            -- We map over the parser and get the names
            -- ( no Effect here though, unlike for completers )
            possibles   = fold $ runExists (\zz -> mapParser opt_completions zz) x

            --
            -- Look at the option and give back the possible
            -- things the user could type. If it's a command
            -- reader also ensure that it can be immediately
            -- reachable from where the error was given.
            opt_completions :: forall g. OptHelpInfo -> Option g -> Array String
            opt_completions (OptHelpInfo hinfo) (Option opt) = case opt.optMain of
              OptReader ns _ _ -> map showOption ns
              FlagReader ns _  -> map showOption ns
              ArgReader _      -> []
              CmdReader _ ns _  | hinfo.hinfoUnreachableArgs
                               -> []
                                | otherwise
                               -> ns
          in suggestions
      _
        -> mempty

    base_help :: forall x. ParserInfo x -> ParserHelp
    base_help (ParserInfo i) =
      if show_full_help
        then fold [h, f, parserHelp pprefs i.infoParser]
        else mempty
      where
        h = headerHelp i.infoHeader
        f = footerHelp i.infoFooter

    show_full_help = case msg of
      ShowHelpText             -> true
      MissingError CmdStart  _  | (un ParserPrefs pprefs).prefShowHelpOnEmpty
                               -> true
      _                        -> (un ParserPrefs pprefs).prefShowHelpOnError

renderFailure :: ParserFailure ParserHelp -> String -> Tuple String ExitCode
renderFailure failure progn =
  let h /\ exit /\ cols /\ unit = un ParserFailure failure progn
  in Tuple (renderHelp cols h) exit
