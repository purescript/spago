
module Options.Applicative.Types (
  ParseError(..),
  ParserInfo(..),
  ParserPrefs(..),

  Option(..),
  OptName(..),
  OptReader(..),
  OptProperties(..),
  OptVisibility(..),
  Backtracking(..),
  ReadM(..),
  readerAsk,
  readerAbort,
  readerError,
  CReader(..),
  Parser(..),
  MultPE(..),
  Completer(..),
  mkCompleter,
  CompletionResult(..),
  ParserFailure(..),
  ParserResult(..),
  overFailure,
  Args,
  ArgPolicy(..),
  OptHelpInfo(..),
  OptTree(..),
  module Reexport,
  SomeParser(..),
  Context(..),
  IsCmdStart(..),

  optVisibility,
  optMetaVar,
  optHelp,
  optShowDefault,
  optDescMod,
  many,
  some,
  optional
  ) where

import Prelude

import Control.Alternative (class Alt, alt, (<|>))
import Control.Monad.Except (Except)
import Control.Monad.Except.Trans (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader.Trans (ReaderT, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, un)
import Data.Tuple.Nested (Tuple3, (/\))
import Effect (Effect)
import ExitCodes (ExitCode)
import Options.Applicative.Help.Chunk (Chunk)
import Options.Applicative.Help.Pretty (Doc)
import Options.Applicative.Help.Types (ParserHelp(..)) as Reexport
import Options.Applicative.Help.Types (ParserHelp)


data ParseError
  = ErrorMsg String
  | InfoMsg String
  | ShowHelpText
  | MissingError IsCmdStart SomeParser
  | ExpectsArgError String
  | UnexpectedError String SomeParser

derive instance isCmdStartGeneric :: Generic IsCmdStart _
instance isCmdStartShow :: Show IsCmdStart where show = genericShow
data IsCmdStart = CmdStart | CmdCont

instance parseErrorSemigroup :: Semigroup ParseError where
  append _ m = m


-- | A 'ParserInfo' describes a command line program, used to generate a help
-- | screen. Two help modes are supported: brief and full. In brief mode, only
-- | an option and argument summary is displayed, while in full mode each
-- | available option and command, including hidden ones, is described.
-- |
-- | A basic 'ParserInfo' with default values for fields can be created using
-- | the 'info' function.
-- A full description for a runnable 'Parser' for a program.
newtype ParserInfo a = ParserInfo
  { infoParser :: Parser a    -- ^ the option parser for the program
  , infoFullDesc :: Boolean      -- ^ whether the help text should contain full
                              -- documentation
  , infoProgDesc :: Chunk Doc -- ^ brief parser description
  , infoHeader :: Chunk Doc   -- ^ header of the full parser description
  , infoFooter :: Chunk Doc   -- ^ footer of the full parser description
  , infoFailureCode :: ExitCode    -- ^ exit code for a parser failure
  , infoPolicy :: ArgPolicy   -- ^ allow regular options and flags to occur
                              -- after arguments (default: InterspersePolicy)
  }

derive instance parserInfoNewtype :: Newtype (ParserInfo a) _
instance parserInfoFunctor :: Functor ParserInfo where
  map f = over ParserInfo \i -> i { infoParser = map f (i.infoParser) }

data Backtracking
  = Backtrack
  | NoBacktrack
  | SubparserInline

derive instance backtrackingEq :: Eq Backtracking
derive instance backtrackingGeneric :: Generic Backtracking _
instance backtrackingShow :: Show Backtracking where show = genericShow

-- | Global preferences for a top-level 'Parser'.
-- | A 'ParserPrefs' contains general preferences for all command-line
-- | options, and can be built with the 'prefs' function.
newtype ParserPrefs = ParserPrefs
  { prefMultiSuffix :: String     -- ^ metavar suffix for multiple options
  , prefDisambiguate :: Boolean      -- ^ automatically disambiguate abbreviations
                                  -- (default: false)
  , prefShowHelpOnError :: Boolean   -- ^ always show help text on parse errors
                                  -- (default: false)
  , prefShowHelpOnEmpty :: Boolean   -- ^ show the help text for a command or subcommand
                                  -- if it fails with no input (default: false)
  , prefBacktrack :: Backtracking -- ^ backtrack to parent parser when a
                                  -- subcommand fails (default: Backtrack)
  , prefColumns :: Int            -- ^ number of columns in the terminal, used to
                                  -- format the help page (default: 80)
  }
derive instance parserPrefsNewtype :: Newtype ParserPrefs _
derive instance parserPrefsEq :: Eq ParserPrefs
derive instance parserPrefsGeneric :: Generic ParserPrefs _
instance parserPrefsShow :: Show ParserPrefs where show = genericShow


data OptName = OptShort Char
             | OptLong String

derive instance optNameEq :: Eq OptName
derive instance optNameOrd :: Ord OptName
derive instance optNameGeneric :: Generic OptName _
instance optNameShow :: Show OptName where show = genericShow


-- | Visibility of an option in the help text.
data OptVisibility
  = Internal          -- ^ does not appear in the help text at all
  | Hidden            -- ^ only visible in the full description
  | Visible           -- ^ visible both in the full and brief descriptions

derive instance optVisibilityEq :: Eq OptVisibility
derive instance optVisibilityOrd :: Ord OptVisibility
derive instance optVisibilityGeneric :: Generic OptVisibility _
instance optVisibilityShow :: Show OptVisibility where show = genericShow

-- | Specification for an individual parser option.
newtype OptProperties = OptProperties
  { propVisibility :: OptVisibility       -- ^ whether this flag is shown in the brief description
  , propHelp :: Chunk Doc                 -- ^ help text for this option
  , propMetaVar :: String                 -- ^ metavariable for this option
  , propShowDefault :: Maybe String       -- ^ what to show in the help text as the default
  , propDescMod :: Maybe ( Doc -> Doc )   -- ^ a function to run over the brief description
  }
derive instance optPropertiesNewtype :: Newtype OptProperties _
instance optPropertiesShow :: Show OptProperties where
  show (OptProperties r) = "(OptProperties " <> show
    { propVisibility: r.propVisibility
    , propHelp: r.propHelp
    , propMetaVar: r.propMetaVar
    , propShowDefault: r.propShowDefault
    , propDescMod: map (const "<func>") r.propDescMod
    } <> ")"

-- | A single option of a parser.
newtype Option a = Option
  { optMain :: OptReader a               -- ^ reader for this option
  , optProps :: OptProperties            -- ^ properties of this option
  }
derive instance optionNewtype :: Newtype (Option a) _
data SomeParser = SomeParser (Exists Parser)

-- | Subparser context, containing the 'name' of the subparser, and its parser info.
-- | Used by parserFailure to display relevant usage information when parsing inside a subparser fails.
data Context = Context String (Exists ParserInfo)

instance optionShow :: Show (Option a) where
  show (Option opt) = "(Option " <> show {optProps: opt.optProps, optMain: "<OptReader>"} <> ")"

instance optionFunctor :: Functor Option where
  map f  = over Option \o -> o{optMain = map f o.optMain}

-- | A reader is used by the 'option' and 'argument' builders to parse
-- | the data passed by the user on the command line into a data type.
-- |
-- | The most common are 'str' which is used for 'String', there are
-- | readers for `Int`, `Number`, `Boolean`.
-- |
-- | More complex types can use the 'eitherReader' or 'maybeReader'
-- | functions to pattern match or use a more expressive parser like a
-- | member of the 'Parsec' family.
-- | A newtype over 'ReaderT String Except', used by option readers.
newtype ReadM a = ReadM (ReaderT String (Except ParseError) a)

derive instance readMNewtype :: Newtype (ReadM a) _
instance readMFunctor :: Functor ReadM where
  map f (ReadM r) = ReadM (map f r)

instance readMApply :: Apply ReadM where
  apply (ReadM x) (ReadM y) = ReadM $ x <*> y

instance readMApplicative :: Applicative ReadM where
  pure = ReadM <<< pure

instance readMAlt :: Alt ReadM where
  alt (ReadM x) (ReadM y) = ReadM $ alt x y

instance readMBind :: Bind ReadM where
  bind (ReadM r) f = ReadM $ r >>= un ReadM <<< f

instance readMMonad :: Monad ReadM

instance readMMonadFail :: MonadThrow String ReadM where
  throwError = readerError

-- | Return the value being read.
readerAsk :: ReadM String
readerAsk = ReadM ask

-- | Abort option reader by exiting with a 'ParseError'.
readerAbort :: forall a. ParseError -> ReadM a
readerAbort = ReadM <<< lift <<< throwError

-- | Abort option reader by exiting with an error message.
readerError :: forall a. String -> ReadM a
readerError = readerAbort <<< ErrorMsg

newtype CReader a = CReader
  { crCompleter :: Completer
  , crReader :: ReadM a }

derive instance newtypeCReader :: Newtype (CReader a) _
instance cReaderFunctor :: Functor CReader where
  map f = over CReader \r -> r {crReader = map f r.crReader}

-- | An 'OptReader' defines whether an option matches an command line argument.
data OptReader a
  = OptReader (Array OptName) (CReader a) (String -> ParseError)
  -- ^ option reader
  | FlagReader (Array OptName) a
  -- ^ flag reader
  | ArgReader (CReader a)
  -- ^ argument reader
  | CmdReader (Maybe String) (Array String) (String -> Maybe (ParserInfo a))
  -- ^ command reader

instance optReaderFunctor :: Functor OptReader where
  map f (OptReader ns cr e) = OptReader ns (map f cr) e
  map f (FlagReader ns x) = FlagReader ns (f x)
  map f (ArgReader cr) = ArgReader (map f cr)
  map f (CmdReader n cs g) = CmdReader n cs ((map <<< map) f <<< g)


-- * Option Parsers
-- |
-- | A 'Parser' is the core type in optparse-applicative. A value of type
-- | Parser a@ represents a specification for a set of options, which will
-- | yield a value of type a when the command line arguments are successfully
-- | parsed.
-- |
-- | There are several types of primitive 'Parser'.
-- |
-- | * Flags: simple no-argument options. When a flag is encountered on the
-- | command line, its value is returned.
-- |
-- | * Options: options with an argument. An option can define a /reader/,
-- | which converts its argument from String to the desired value, or throws a
-- | parse error if the argument does not validate correctly.
-- |
-- | * Arguments: positional arguments, validated in the same way as option
-- | arguments.
-- |
-- | * Commands. A command defines a completely independent sub-parser. When a
-- | command is encountered, the whole command line is passed to the
-- | corresponding parser.
-- |
-- | ** Parser builders
-- |
-- | Each parser builder takes an option modifier. A modifier can be created by
-- | composing the basic modifiers provided by here using the 'Monoid' operations
-- | mempty' and 'append', or their aliases 'idm' and '<>'.
-- |
-- | For example:
-- |
-- | > out = strOption
-- | >     ( long "output"
-- | >    <> short 'o'
-- | >    <> metavar "FILENAME" )
-- |
-- | creates a parser for an option called \"output\".
data Parser a
  = NilP a
  | OptP (Option a)
  | MultP (Exists (MultPE a))
  | AltP (Parser a) (Parser a)
  | BindP (Free Parser a)

data MultPE a x = MultPE (Parser (x -> a)) (Parser x)

instance parserFunctor :: Functor Parser where
  map f (NilP x) = NilP (f x)
  map f (OptP opt) = OptP (map f opt)
  map f (MultP e) = runExists (\(MultPE p1 p2) -> MultP $ mkExists $ MultPE (map (f <<< _) p1) p2) e
  map f (AltP p1 p2) = AltP (map f p1) (map f p2)
  map f (BindP e) = BindP $ map f e

instance parserApply :: Apply Parser where
  apply a b = MultP (mkExists (MultPE a b))

instance parserApplicative :: Applicative Parser where
  pure = NilP

instance parserAlt :: Alt Parser where
  alt = AltP

newtype ParserM a = ParserM (Free Parser a)

derive newtype instance parserMFunctor :: Functor ParserM
derive newtype instance parserMBind :: Bind ParserM
derive newtype instance parserMApply :: Apply ParserM
derive newtype instance parserMApplicative :: Applicative ParserM
derive newtype instance parserMMonad :: Monad ParserM
derive newtype instance parserMMonadRec :: MonadRec ParserM

fromM :: forall a. ParserM a -> Parser a
fromM (ParserM f) = BindP f

oneM :: forall a. Parser a -> ParserM a
oneM = liftF >>> ParserM

manyM :: forall a. Parser a -> ParserM (List a)
manyM p = tailRecM go List.Nil
  where
  go :: List a -> ParserM (Step (List a) (List a))
  go acc = do
    aa <- oneM $ (Loop <$> p) <|> pure (Done unit)
    pure $ bimap (_ List.: acc) (\_ -> List.reverse acc) aa

someM :: forall a. Parser a -> ParserM (NonEmptyList a)
someM p = NEL.cons' <$> oneM p <*> manyM p

-- | Parses 0 or more values using the given parser. **Note: this should
-- | never be used with the `value` modifier.**
-- |
-- | For example, by using this option
-- | `many (strOption (long "arg-name"))`
-- |
-- | one could write
-- | ```
-- | command
-- | # produces Nil
-- |
-- | command --arg-name first
-- | # produces ("first" : Nil)
-- |
-- | command --arg-name first --arg-name second
-- | # produces ("first" : "second" : Nil)
-- | ```
-- |
-- | To parse 1 or more values, see `some` instead.
many :: forall a. Parser a -> Parser (List a)
many = manyM >>> fromM

-- | Parses 1 or more values using the given parser. **Note: this should
-- | never be used with the `value` modifier.**
-- |
-- | For example, by using this option
-- | `some (strOption (long "arg-name"))`
-- |
-- | one could write
-- | ```
-- | command
-- | # produces failure message
-- |
-- | command --arg-name first
-- | # produces (NonEmptyList "first" Nil)
-- |
-- | command --arg-name first --arg-name second
-- | # produces (NonEmptyList "first" ("second" : Nil))
-- | ```
-- |
-- | To parse 0 or more values, see `many` instead.
some :: forall a. Parser a -> Parser (NonEmptyList a)
some = someM >>> fromM

optional :: forall f a. Alt f => Applicative f => f a -> f (Maybe a)
optional a = map Just a <|> pure Nothing

-- | optparse-applicative supplies a rich completion system for bash,
-- | zsh, and fish shells.
-- |
-- | 'Completer' functions are used for option and argument to complete
-- | their values.
-- |
-- | Use the 'completer' builder to use these.
-- | The 'action' and 'completeWith' builders are also provided for
-- | convenience, to use 'bashCompleter' and 'listCompleter' as a 'Mod'.
newtype Completer = Completer (String -> Effect (Array String))
derive instance newtypeCompleter :: Newtype Completer _
-- | Smart constructor for a 'Completer'
mkCompleter :: (String -> Effect (Array String)) -> Completer
mkCompleter = Completer

instance completerSemigroup :: Semigroup Completer where
  append (Completer c1) (Completer c2) =
    Completer $ \s -> (<>) <$> c1 s <*> c2 s

instance completerMonoid :: Monoid Completer where
  mempty = Completer $ \_ -> pure []

newtype CompletionResult = CompletionResult
  { execCompletion :: String -> Effect String }

derive instance newtypeCompletionResult :: Newtype CompletionResult _
instance completionResultShow :: Show CompletionResult where
  show _ = "(CompletionResult <function>)"

newtype ParserFailure h = ParserFailure (String -> Tuple3 h ExitCode Int)

derive instance newtypeParserFailure :: Newtype (ParserFailure h) _

instance parserFailureShow :: Show (ParserFailure h) where
  show _ = "(ParserFailure <function>)"

instance parserFailureFunctor :: Functor ParserFailure where
  map f (ParserFailure err) = ParserFailure $ \progn ->
    let  h /\ exit /\ cols /\ unit = err progn
    in f h /\ exit /\ cols /\ unit

-- | Result of 'execParserPure'.
data ParserResult a
  = Success a
  | Failure (ParserFailure ParserHelp)
  | CompletionInvoked CompletionResult
derive instance parserResultGeneric :: Generic (ParserResult a) _
instance parserResultShow :: Show a => Show (ParserResult a) where show = genericShow

instance parserResultFunctor :: Functor ParserResult where
  map f (Success a) = Success (f a)
  map _ (Failure f) = Failure f
  map _ (CompletionInvoked c) = CompletionInvoked c

overFailure :: forall a. (ParserHelp -> ParserHelp)
            -> ParserResult a -> ParserResult a
overFailure f (Failure failure) = Failure $ map f failure
overFailure _ r = r

instance parserResultApply :: Apply ParserResult where
  apply (Success f) r = map f r
  apply (Failure f) _ = Failure f
  apply (CompletionInvoked c) _ = CompletionInvoked c

instance parserResultApplicative :: Applicative ParserResult where
  pure = Success

instance parserResultBind :: Bind ParserResult where
  bind (Success x) f = f x
  bind (Failure f) _ = Failure f
  bind (CompletionInvoked c) _ = CompletionInvoked c

instance parserResultMonad :: Monad ParserResult

type Args = List String

-- | Policy for how to handle options within the parse
data ArgPolicy
  = Intersperse
  -- ^ The default policy, options and arguments can
  --   be interspersed.
  --   A `--` option can be passed to ensure all following
  --   commands are treated as arguments.
  | NoIntersperse
  -- ^ Options must all come before arguments, once a
  --   single positional argument or subcommand is parsed,
  --   all remaining arguments are treated as positionals.
  --   A `--` option can be passed if the first positional
  --   one needs starts with `-`.
  | AllPositionals
  -- ^ No options are parsed at all, all arguments are
  --   treated as positionals.
  --   Is the policy used after `--` is encountered.
  | ForwardOptions
  -- ^ Options and arguments can be interspersed, but if
  --   a given option is not found, it is treated as a
  --   positional argument. This is sometimes useful if
  --   one is passing through most options to another tool,
  --   but are supplying just a few of their own options.

derive instance argPolicyEq :: Eq ArgPolicy
derive instance argPolicyOrd :: Ord ArgPolicy
derive instance argPolicyGeneric :: Generic ArgPolicy _
instance argPolicyShow :: Show ArgPolicy where show = genericShow


newtype OptHelpInfo = OptHelpInfo
  { hinfoMulti :: Boolean    -- ^ Whether this is part of a many or some (approximately)
  , hinfoDefault :: Boolean  -- ^ Whether this option has a default value
  , hinfoUnreachableArgs :: Boolean -- ^ If the result is a positional, if it can't be
                                 --   accessed in the current parser position ( first arg )
  }
derive instance newtypeOptHelpInfo :: Newtype OptHelpInfo _
derive instance optHelpInfoEq :: Eq OptHelpInfo
derive instance optHelpInfoGeneric :: Generic OptHelpInfo _
instance optHelpInfoShow :: Show OptHelpInfo where show = genericShow


data OptTree a
  = Leaf a
  | MultNode (Array (OptTree a))
  | AltNode (Array (OptTree a))

derive instance optTreeGeneric :: Generic (OptTree a) _
instance optTreeShow :: Show a => Show (OptTree a) where show = genericShow

optVisibility :: forall a. Option a -> OptVisibility
optVisibility = _.propVisibility <<< un OptProperties <<< _.optProps <<< un Option

optHelp :: forall a. Option a -> Chunk Doc
optHelp  = _.propHelp <<< un OptProperties <<< _.optProps <<< un Option

optMetaVar :: forall a. Option a -> String
optMetaVar = _.propMetaVar <<< un OptProperties <<< _.optProps <<< un Option

optShowDefault :: forall a. Option a -> Maybe String
optShowDefault = _.propShowDefault <<< un OptProperties <<< _.optProps <<< un Option

optDescMod :: forall a. Option a -> Maybe ( Doc -> Doc )
optDescMod = _.propDescMod <<< un OptProperties <<< _.optProps <<< un Option
