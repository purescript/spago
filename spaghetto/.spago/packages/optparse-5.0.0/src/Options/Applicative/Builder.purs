  -- * Parser builders
  --
  -- | This module contains utility functions and combinators to create parsers
  -- | for individual options.
  -- |
  -- | Each parser builder takes an option modifier. A modifier can be created by
  -- | composing the basic modifiers provided by this module using the 'Monoid'
  -- | operations 'mempty' and 'append', or their aliases 'idm' and '<>'.
  -- |
  -- | For example:
  -- |
  -- | ```purescript
  -- | out = strOption
  -- |     ( long "output"
  -- |    <> short 'o'
  -- |    <> metavar "FILENAME" )
  -- | ```
  -- |
  -- | creates a parser for an option called `output`.
module Options.Applicative.Builder
  ( subparser
  , strArgument
  , argument
  , flag
  , flag'
  , switch
  , abortOption
  , infoOption
  , strOption
  , option

  -- * Modifiers
  , short
  , long
  , help
  , helpDoc
  , value
  , showDefaultWith
  , showDefault
  , metavar
  , noArgError
  , hidden
  , style
  , command
  , commandGroup
  , completeWith
  , action
  , completer
  , idm

  -- * Readers
  --
  -- | A collection of basic 'Option' readers.
  , str
  , int
  , number
  , boolean
  , maybeReader
  , eitherReader
  , disabled

  -- * Builder for 'ParserInfo'
  , InfoMod(..)
  , fullDesc
  , briefDesc
  , header
  , headerDoc
  , footer
  , footerDoc
  , progDesc
  , progDescDoc
  , failureCode
  , noIntersperse
  , forwardOptions
  , info

  -- * Builder for 'ParserPrefs'
  , PrefsMod(..)
  , multiSuffix
  , disambiguate
  , showHelpOnError
  , showHelpOnEmpty
  , noBacktrack
  , subparserInline
  , columns
  , prefs
  , defaultPrefs

  -- * Types
  , module  Reexport
  ) where


import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, un)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import ExitCodes (ExitCode)
import ExitCodes as ExitCode
import Data.Number as Number
import Options.Applicative.Builder.Completer (bashCompleter, listCompleter)
import Options.Applicative.Builder.Internal (class HasCompleter, class HasMetavar, class HasName, class HasValue, ArgumentFields(..), CommandFields(..), DefaultProp(..), FlagFields(..), Mod(..), OptionFields(..), fieldMod, mkCommand, mkParser, modCompleter, name, optionMod)
import Options.Applicative.Builder.Internal (internal, Mod, OptionFields, FlagFields, ArgumentFields, CommandFields, class HasName, class HasCompleter, class HasValue, class HasMetavar) as Reexport
import Options.Applicative.Help.Chunk (Chunk(..), paragraph)
import Options.Applicative.Help.Pretty (Doc)
import Options.Applicative.Types (ArgPolicy(..), Backtracking(..), CReader(..), Completer, OptName(..), OptProperties(..), OptReader(..), OptVisibility(..), ParseError(..), Parser, ParserInfo(..), ParserPrefs(..), ReadM, readerAbort, readerAsk, readerError)
import Options.Applicative.Types (ParseError(..), readerAbort, readerError, ReadM) as Reexport

-- Readers --

-- | String 'Option' reader.
str :: ReadM String
str = readerAsk

-- | Int 'Option' reader.
int :: ReadM Int
int = eitherReader $ \s -> case Int.fromString s of
  Nothing -> Left $ "Can't parse as Int: `" <> show s <> "`"
  Just a -> Right a

-- | Number 'Option' reader.
number :: ReadM Number
number = eitherReader $ \s -> case Number.fromString s of
  Nothing -> Left $ "Can't parse as Number: `" <> show s <> "`"
  Just a -> Right a

-- | Boolean 'Option' reader.
boolean :: ReadM Boolean
boolean = eitherReader $ toLower >>> case _ of
  "true" -> Right true
  "false" -> Right false
  s -> Left $ "Can't parse as Boolean: `" <> show s <> "`"

-- | Convert a function producing an 'Either' into a reader.
eitherReader :: forall a. (String -> Either String a) -> ReadM a
eitherReader f = readerAsk >>= either readerError pure <<< f

-- | Convert a function producing a 'Maybe' into a reader.
maybeReader :: forall a. (String -> Maybe a) -> ReadM a
maybeReader f = do
  arg  <- readerAsk
  maybe (readerError $ "cannot parse value `" <> arg <> "'") pure <<< f $ arg

-- | Null 'Option' reader. All arguments will fail validation.
disabled :: forall a. ReadM a
disabled = readerError "disabled option"

-- modifiers --

-- | Specify a short name for an option.
short :: forall f a. HasName f => Char -> Mod f a
short = fieldMod <<< name <<< OptShort

-- | Specify a long name for an option.
long :: forall f a. HasName f => String -> Mod f a
long = fieldMod <<< name <<< OptLong

-- | Specify a default value for an option.
-- |
-- | **Note**: Because this modifier means the parser will never fail,
-- | do not use it with combinators such as 'some' or 'many', as
-- | these combinators continue until a failure occurs.
-- | Careless use will thus result in a hang.
-- |
-- | To display the default value, combine with `showDefault` or
-- | `showDefaultWith`.
value :: forall f a. HasValue f => a -> Mod f a
value x = Mod identity (DefaultProp (Just x) Nothing) identity

-- | Show the default value for this option using a function.
showDefaultWith :: forall f a. (a -> String) -> Mod f a
showDefaultWith s = Mod identity (DefaultProp Nothing (Just s)) identity

-- | Show the default value for this option using its 'Show' instance.
showDefault :: forall f a. Show a => Mod f a
showDefault = showDefaultWith show

-- | Specify the help text for an option.
help :: forall a f. String -> Mod f a
help s = optionMod $ over OptProperties \p -> p { propHelp = paragraph s }

-- | Specify the help text for an option as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- | value.
helpDoc :: forall f a. Maybe Doc -> Mod f a
helpDoc doc = optionMod $ over OptProperties \p -> p { propHelp = Chunk doc }

-- | Specify the error to display when no argument is provided to this option.
noArgError :: forall a. ParseError -> Mod OptionFields a
noArgError e = fieldMod $ over OptionFields \p -> p { optNoArgError = const e }

-- | Specify a metavariable for the argument.
-- |
-- | Metavariables have no effect on the actual parser, and only serve to specify
-- | the symbolic name for an argument to be displayed in the help text.
metavar :: forall f a. HasMetavar f => String -> Mod f a
metavar var = optionMod $ over OptProperties \p -> p { propMetaVar = var }

-- | Hide this option from the brief description.
hidden :: forall f a. Mod f a
hidden = optionMod $ over OptProperties \p ->
  p { propVisibility = min Hidden p.propVisibility }

-- | Apply a function to the option description in the usage text.
-- |
-- | > import Options.Applicative.Help
-- | > flag' () (short 't' <> style bold)
-- |
-- | **NOTE**: This builder is more flexible than its name and example
-- | allude. One of the motivating examples for its addition was to
-- | used `const` to completely replace the usage text of an option.
style :: forall f a. ( Doc -> Doc ) -> Mod f a
style x = optionMod $ over OptProperties \p ->
  p { propDescMod = Just x }

-- | Add a command to a subparser option.
-- |
-- | Suggested usage for multiple commands is to add them to a single subparser. e.g.
-- |
-- | ```purescript
-- | sample :: Parser Sample
-- | sample = subparser
-- |        ( command "hello"
-- |          (info hello (progDesc "Print greeting"))
-- |       <> command "goodbye"
-- |          (info goodbye (progDesc "Say goodbye"))
-- |        )
-- | ```
command :: forall a. String -> ParserInfo a -> Mod CommandFields a
command cmd pinfo = fieldMod $ over CommandFields \p ->
  p { cmdCommands = [Tuple cmd pinfo] <> p.cmdCommands }

-- | Add a description to a group of commands.
-- |
-- | Advanced feature for separating logical groups of commands on the parse line.
-- |
-- | If using the same `metavar` for each group of commands, it may yield a more
-- | attractive usage text combined with `hidden` for some groups.
commandGroup :: forall a. String -> Mod CommandFields a
commandGroup g = fieldMod $ over CommandFields \p ->
  p { cmdGroup = Just g }

-- | Add a list of possible completion values.
completeWith :: forall f a. HasCompleter f => Array String -> Mod f a
completeWith = completer <<< listCompleter

-- | Add a bash completion action. Common actions include `file` and
-- | `directory`. See
-- | <http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins>
-- | for a complete list.
action :: forall f a. HasCompleter f => String -> Mod f a
action = completer <<< bashCompleter

-- | Add a completer to an argument.
-- |
-- | A completer is a function `String -> Effect String` which, given a partial
-- | argument, returns all possible completions for that argument.
completer :: forall f a. HasCompleter f => Completer -> Mod f a
completer f = fieldMod $ modCompleter (_ `append` f)

-- parsers --

-- | Builder for a command parser. The 'command' modifier can be used to
-- | specify individual commands.
subparser :: forall a. Mod CommandFields a -> Parser a
subparser m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" `append` m
    groupName /\ cmds /\ subs /\ unit = mkCommand m
    rdr = CmdReader groupName cmds subs

-- | Builder for an argument parser.
argument :: forall a. ReadM a -> Mod ArgumentFields a -> Parser a
argument p (Mod f d g) = mkParser d g (ArgReader rdr)
  where
    ArgumentFields compl = f (ArgumentFields mempty)
    rdr = CReader {crCompleter: compl.argCompleter, crReader: p}

-- | Builder for a 'String' argument.
strArgument :: Mod ArgumentFields String -> Parser String
strArgument = argument str

-- | Builder for a flag parser.
-- |
-- | A flag that switches from a \"default value\" to an \"active value\" when
-- | encountered. For a simple boolean value, use `switch` instead.
-- |
-- | **Note**: Because this parser will never fail, it can not be used with
-- | combinators such as 'some' or 'many', as these combinators continue until
-- | a failure occurs. See @flag'@.
flag :: forall a
      . a                         -- ^ default value
     -> a                         -- ^ active value
     -> Mod FlagFields a          -- ^ option modifier
     -> Parser a
flag defv actv m = flag' actv m <|> pure defv

-- | Builder for a flag parser without a default value.
-- |
-- | Same as 'flag', but with no default value. In particular, this flag will
-- | never parse successfully by itself.
-- |
-- | It still makes sense to use it as part of a composite parser. For example
-- |
-- | > length <$> many (flag' () (short 't'))
-- |
-- | is a parser that counts the number of "-t" arguments on the command line,
-- | alternatively
-- |
-- | > flag' true (long "on") <|> flag' false (long "off")
-- |
-- | will require the user to enter '--on' or '--off' on the command line.
flag' :: forall a
       . a                         -- ^ active value
      -> Mod FlagFields a          -- ^ option modifier
      -> Parser a
flag' actv (Mod f d g) = mkParser d g rdr
  where
    rdr = let (FlagFields fields) = f (FlagFields {flagNames: [], flagActive: actv})
          in FlagReader (fields.flagNames)
                        (fields.flagActive)

-- | Builder for a boolean flag.
-- |
-- | **Note**: Because this parser will never fail, it can not be used with
-- | combinators such as 'some' or 'many', as these combinators continue until
-- | a failure occurs. See @flag'@.
-- |
-- | > switch = flag false true
switch :: Mod FlagFields Boolean -> Parser Boolean
switch = flag false true

-- | An option that always fails.
-- |
-- | When this option is encountered, the option parser immediately aborts with
-- | the given parse error.  If you simply want to output a message, use
-- | 'infoOption' instead.
abortOption :: forall a. ParseError -> Mod OptionFields (a -> a) -> Parser (a -> a)
abortOption err m = option (readerAbort err) <<< (_ `append` m) $ fold
  [ noArgError err
  , value identity
  , metavar "" ]

-- | An option that always fails and displays a message.
infoOption :: forall a. String -> Mod OptionFields (a -> a) -> Parser (a -> a)
infoOption = abortOption <<< InfoMsg

-- | Builder for an option taking a 'String' argument.
strOption :: Mod OptionFields String -> Parser String
strOption = option str


-- | Builder for an option using the given reader.
-- |
-- | This is a regular option, and should always have either a `long` or
-- | `short` name specified in the modifiers (or both).
-- |
-- | > nameParser = option str ( long "name" <> short 'n' )
option :: forall a. ReadM a -> Mod OptionFields a -> Parser a
option r m = mkParser d g rdr
  where
    Mod f d g = metavar "ARG" `append` m
    (OptionFields fields) = f (OptionFields {optNames: [], optCompleter: mempty, optNoArgError: ExpectsArgError})
    crdr = CReader {crCompleter: fields.optCompleter, crReader: r}
    rdr = OptReader fields.optNames crdr fields.optNoArgError

-- | Modifier for 'ParserInfo'.
newtype InfoMod a = InfoMod (ParserInfo a -> ParserInfo a)
derive instance newtypeInfoMod :: Newtype (InfoMod a) _

instance infoModMonoid :: Monoid (InfoMod a) where
  mempty = InfoMod identity

instance infoModSemigroup :: Semigroup (InfoMod a) where
  append m1 m2 = InfoMod $ un InfoMod m2 <<< un InfoMod m1

-- | Show a full description in the help text of this parser (i.e.
-- | options with the `hidden` modifier will still be displayed,
-- | unlike `briefDesc`).
fullDesc :: forall a. InfoMod a
fullDesc = InfoMod $ over ParserInfo \i -> i { infoFullDesc = true }

-- | Only show a brief description in the help text of this parser (i.e.
-- | options with the `hidden` modifier will NOT be displayed,
-- | unlike `fullDesc`).
briefDesc :: forall a. InfoMod a
briefDesc = InfoMod $ over ParserInfo \i -> i { infoFullDesc = false }

-- | Specify a header for this parser.
header :: forall a. String -> InfoMod a
header s = InfoMod $ over ParserInfo \i -> i { infoHeader = paragraph s }

-- | Specify a header for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- | value.
headerDoc :: forall a. Maybe Doc -> InfoMod a
headerDoc doc = InfoMod $ over ParserInfo \i -> i { infoHeader = Chunk doc }

-- | Specify a footer for this parser.
footer :: forall a. String -> InfoMod a
footer s = InfoMod $ over ParserInfo \i -> i { infoFooter = paragraph s }

-- | Specify a footer for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- | value.
footerDoc :: forall a. Maybe Doc -> InfoMod a
footerDoc doc = InfoMod $ over ParserInfo \i -> i { infoFooter = Chunk doc }

-- | Specify a short program description.
progDesc :: forall a. String -> InfoMod a
progDesc s = InfoMod $ over ParserInfo \i -> i { infoProgDesc = paragraph s }

-- | Specify a short program description as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- | value.
progDescDoc :: forall a. Maybe Doc -> InfoMod a
progDescDoc doc = InfoMod $ over ParserInfo \i -> i { infoProgDesc = Chunk doc }

-- | Specify an exit code if a parse error occurs.
failureCode :: forall a. ExitCode -> InfoMod a
failureCode n = InfoMod $ over ParserInfo \i -> i { infoFailureCode = n }

-- | Disable parsing of regular options after arguments. After a positional
-- | argument is parsed, all remaining options and arguments will be treated
-- | as a positional arguments. Not recommended in general as users often
-- | expect to be able to freely intersperse regular options and flags within
-- | command line options.
noIntersperse :: forall a. InfoMod a
noIntersperse = InfoMod $ over ParserInfo \p -> p { infoPolicy = NoIntersperse }

-- | Intersperse matched options and arguments normally, but allow unmatched
-- | options to be treated as positional arguments.
-- | This is sometimes useful if one is wrapping a third party cli tool and
-- | needs to pass options through, while also providing a handful of their
-- | own options. Not recommended in general as typos by the user may not
-- | yield a parse error and cause confusion.
forwardOptions :: forall a. InfoMod a
forwardOptions = InfoMod $ over ParserInfo \p -> p { infoPolicy = ForwardOptions }

-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: forall a. Parser a -> InfoMod a -> ParserInfo a
info parser m = un InfoMod m base
  where
    base = ParserInfo
      { infoParser: parser
      , infoFullDesc: true
      , infoProgDesc: mempty
      , infoHeader: mempty
      , infoFooter: mempty
      , infoFailureCode: ExitCode.Error
      , infoPolicy: Intersperse }

newtype PrefsMod = PrefsMod (ParserPrefs -> ParserPrefs)
derive instance newtypePrefsMod :: Newtype PrefsMod _

instance prefsModMonoid :: Monoid PrefsMod where
  mempty = PrefsMod identity

instance prefsModSemigroup :: Semigroup PrefsMod where
  append m1 m2 = PrefsMod $ un PrefsMod m2 <<< un PrefsMod m1

-- | Include a suffix to attach to the metavar when multiple values
-- | can be entered.
multiSuffix :: String -> PrefsMod
multiSuffix s = PrefsMod $ over ParserPrefs \p -> p { prefMultiSuffix = s }

-- | Turn on disambiguation.
-- |
-- | See
-- | https://github.com/pcapriotti/optparse-applicative#disambiguation
disambiguate :: PrefsMod
disambiguate = PrefsMod $ over ParserPrefs \p -> p { prefDisambiguate = true }

-- | Show full help text on any error.
showHelpOnError :: PrefsMod
showHelpOnError = PrefsMod $ over ParserPrefs \p -> p { prefShowHelpOnError = true }

-- | Show the help text if the user enters only the program name or
-- | subcommand.
-- |
-- | This will suppress a "Missing:" error and show the full usage
-- | instead if a user just types the name of the program.
showHelpOnEmpty :: PrefsMod
showHelpOnEmpty = PrefsMod $ over ParserPrefs \p -> p { prefShowHelpOnEmpty = true }

-- | Turn off backtracking after subcommand is parsed.
noBacktrack :: PrefsMod
noBacktrack = PrefsMod $ over ParserPrefs \p -> p { prefBacktrack = NoBacktrack }

-- | Allow full mixing of subcommand and parent arguments by inlining
-- | selected subparsers into the parent parser.
-- |
-- | **Note:** When this option is used, preferences for the subparser which
-- | effect the parser behaviour (such as noIntersperse) are ignored.
subparserInline :: PrefsMod
subparserInline = PrefsMod $ over ParserPrefs \p -> p { prefBacktrack = SubparserInline }

-- | Set the maximum width of the generated help text.
columns :: Int -> PrefsMod
columns cols = PrefsMod $ over ParserPrefs \p -> p { prefColumns = cols }

-- | Create a `ParserPrefs` given a modifier
prefs :: PrefsMod -> ParserPrefs
prefs m = un PrefsMod m base
  where
    base = ParserPrefs
      { prefMultiSuffix: ""
      , prefDisambiguate: false
      , prefShowHelpOnError: false
      , prefShowHelpOnEmpty: false
      , prefBacktrack: Backtrack
      , prefColumns: 80 }

-- Convenience shortcuts

-- | Trivial option modifier.
idm :: forall m. Monoid m => m
idm = mempty

-- | Default preferences.
defaultPrefs :: ParserPrefs
defaultPrefs = prefs idm
